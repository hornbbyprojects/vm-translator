;;;; vm-translator.lisp

(in-package #:vm-translator)

(cl-interpol:enable-interpol-syntax)


(defparameter *comment-asm* t)
(defun gen-comment (l) (if *comment-asm* (format nil "//~a~&" l) ""))
;; Command classes not created by macros
(defclass command () ())

(defmacro def-simple-push (push-classname pop-classname)
  `(progn
     (defclass ,push-classname (command)
       ((offset :initarg :offset :reader push-offset :type integer)))
     (defclass ,pop-classname (command)
       ((offset :initarg :offset :reader pop-offset :type integer)))))

(def-simple-push push-constant pop-constant)
(def-simple-push push-arg pop-arg)
(def-simple-push push-local pop-local)
(def-simple-push push-this pop-this)
(def-simple-push push-that pop-that)
(def-simple-push push-temp pop-temp)
(def-simple-push push-pointer pop-pointer)
(def-simple-push push-static pop-static)

;;;; The code generator

;; Arithmetic commands
(defun unary-command (end-op)
  (format nil "~a@SP
A=M-1
M=~a" (gen-comment "UNARY OP") end-op))

(defun binary-command (end-op)
  "Compress last 2 items on stack into end-op, where D is last and M is second last"
  (format nil "~a@SP
M=M-1
A=M
D=M
A=A-1
M=~a" (gen-comment "BINARY OP") end-op))

(defclass codegen-context ()
  ((local-label-counter :initform 0)
   (static-prefix :initarg :static-prefix)))


(defmacro def-binary (name end-op)
  `(progn
     (defclass ,name (command) ())
     (defmethod gen-asm ((command ,name) context) (declare (ignore context)) (binary-command ,end-op))))

(defmacro def-unary (name end-op)
  `(progn
     (defclass ,name (command) ())
     (defmethod gen-asm ((command ,name) context) (declare (ignore context)) (unary-command ,end-op))))


(def-binary add-command "D+M")
(def-binary sub-command "M-D")
(def-unary neg-command "-M")

(def-binary and-command "D&M")
(def-binary or-command "D|M")
(def-unary not-command "!M")


(defun get-local-label (codegen-context label)
  (with-slots (local-label-counter) codegen-context
    (let ((ret local-label-counter))
      (incf local-label-counter)
      (format nil "~a.~a" label ret))))

(defun get-static-prefix (codegen-context offset)
  (with-slots (static-prefix) codegen-context
    #?"$(static-prefix).$(offset)"))


(defun gen-comp-asm-f (jump-type context)
  (let ((comment (gen-comment "COMP"))
        (label (get-local-label context jump-type)))
    #?"$(comment)@SP
$(#|decrement the stack pointer to point at the second element of the comparison|#"")\
M=M-1
A=M
D=M
A=A-1
$(#| Subtract the first element|#"")\
D=M-D
$(#| Set ret to true|#"")\
M=-1
@$(label)
D;$(jump-type)
@SP
A=M-1
$(#| Set ret to false|#"")\
M=0
($(label))"))

(defmacro gen-comp-asm (command-type jump-type)
  `(progn
     (defclass ,command-type (command) ())
     (defmethod gen-asm ((command ,command-type) context)
       (gen-comp-asm-f ,jump-type context))))

(gen-comp-asm eq-command "JEQ")
(gen-comp-asm lt-command "JLT")
(gen-comp-asm gt-command "JGT")


;; Memory manipulation commands
(defun pushd ()
  "Push the current value of the D register onto the stack"
  "@SP
M=M+1
A=M-1
M=D")

(defun pop-into-pointed-by-d ()
  "asm to pop the current top of the stack to the location pointed to by D"
  "@R14
M=D
@SP
M=M-1
A=M
D=M
@R14
A=M
M=D")

(defmethod gen-asm ((command push-constant) context)
  #?"$((gen-comment "PUSH CONSTANT"))\
@$((push-offset command))
D=A
$((pushd))")

(defmacro simple-push (push-classname pop-classname register)
  `(progn
     (defmethod gen-asm ((command ,pop-classname) context)
       #?"$((gen-comment "POP"))\
@$(,register)
D=M
@$((pop-offset command))
D=A+D
$((pop-into-pointed-by-d))")
     (defmethod gen-asm ((command ,push-classname) context)
       #?"$((gen-comment "PUSH"))\
@$(,register) D=M @$((push-offset command)) A=A+D
D=M
$((pushd))")))

(simple-push push-arg pop-arg "ARG")
(simple-push push-local pop-local "LCL")
(simple-push push-this pop-this "THIS")
(simple-push push-that pop-that "THAT")

(defmacro gen-static-asm (push-classname pop-classname offset)
  `(progn
     (defmethod gen-asm ((command ,push-classname) context)
       #?"$((gen-comment "PUSH static"))\
@$((+ ,offset (push-offset command)))
D=M
$((pushd))")

     (defmethod gen-asm ((command ,pop-classname) context)
       #?"$((gen-comment "POP static"))\
@SP
M=M-1
A=M
D=M
@$((+ ,offset (pop-offset command)))
M=D")))

(gen-static-asm push-temp pop-temp 5)
(gen-static-asm push-pointer pop-pointer 3)
;; The parser

(defmethod gen-asm ((command push-static) context)
  #?"@$((get-static-prefix context (push-offset command)))
D=M
$((pushd))")

(defmethod gen-asm ((command pop-static) context)
  #?"@SP
M=M-1
A=M
D=M
@$((get-static-prefix context (pop-offset command)))
M=D")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun map-special-form (symbol mapping form)
    (labels
        ((worker (form)
           (cond
             ((atom form) form)
             ((equal (car form) symbol) (funcall mapping form))
             (t (mapcar #'worker form)))))
      (worker form))))


(defmacro expand-simple-push (form)
  (labels
      ((expand-simple-push (form)
         (trivia:let-match
             (((list _ name classname) form))
           `((equal ,name) (make-instance ,classname :offset index)))))
    (map-special-form 'simple-push #'expand-simple-push form)))


(defun parse-push (segment index)
  (expand-simple-push
   (trivia:match segment
     (simple-push "constant" 'push-constant)
     (simple-push "local" 'push-local)
     (simple-push "argument" 'push-arg)
     (simple-push "this" 'push-this)
     (simple-push "that" 'push-that)
     (simple-push "temp" 'push-temp)
     (simple-push "pointer" 'push-pointer)
     (simple-push "static" 'push-static)
     (otherwise (error #?"Unknown segment $(segment) for push")))))

(defun parse-pop (segment index)
  (expand-simple-push
   (trivia:match segment
     (simple-push "local" 'pop-local)
     (simple-push "argument" 'pop-arg)
     (simple-push "this" 'pop-this)
     (simple-push "that" 'pop-that)
     (simple-push "temp" 'pop-temp)
     (simple-push "pointer" 'pop-pointer)
     (simple-push "static" 'pop-static)
     (otherwise (error #?"Unknown segment $(segment) for pop")))))

(defmacro expand-simple-lines (form)
  (labels
      ((expand-simple-line (form)
         (trivia:let-match
             (((list _ name classname) form))
           `((list (equal ,name)) (make-instance ,classname)))))
    (map-special-form 'simple-line #'expand-simple-line form)))

(defun parse-line (line)
  (let ((words (str:words line)))
    (expand-simple-lines
     (trivia:match words
       (simple-line "add" 'add-command)
       (simple-line "sub" 'sub-command)
       (simple-line "neg" 'neg-command)
       (simple-line "eq" 'eq-command)
       (simple-line "gt" 'gt-command)
       (simple-line "lt" 'lt-command)
       (simple-line "and" 'and-command)
       (simple-line "or" 'or-command)
       (simple-line "not" 'not-command)
       ((list (equal "push") segment index) (parse-push segment (parse-integer index)))
       ((list (equal "pop") segment index) (parse-pop segment (parse-integer index)))
       (otherwise (error (format nil "Unrecognised vm line for vm v1.0 ~a" words)))))))

(defun make-context (static-prefix) (make-instance 'codegen-context :static-prefix static-prefix))

(defun get-static-prefix-for-filename (filename)
  (cl-ppcre:regex-replace ".vm$" (file-namestring filename) ""))

(defun get-out-filename (filename)
  (cl-ppcre:regex-replace ".vm$" filename ".asm"))

(defun read-line-stripping-comments (in)
  (i:iterate
    (i:for line next (read-line in))
    (i:for stripped = (str:trim line))
    (i:for decommented = (cl-ppcre:regex-replace "//.*$" stripped ""))
    (when (not (str:emptyp decommented))
      (i:leave decommented))))

(defun translate-file (filename)
  (with-open-file (in filename)
    (with-open-file (out (get-out-filename filename) :direction :output :if-exists :supersede :if-does-not-exist :create)
      (i:iterate
        (i:with context = (make-context (get-static-prefix-for-filename filename)))
        (i:for line next (handler-case (read-line-stripping-comments in)
                           (end-of-file () (i:terminate))))
        (i:for command = (parse-line line))
        (i:for asm = (handler-case (gen-asm command context)
                       (error (c) (error #?"Caught error $(c) when generating asm for line $(line)~&"))))
        (write-line asm out)))))

(defun main ()
  (translate-file "~/nand2tetris/projects/07/MemoryAccess/BasicTest/BasicTest.vm"))
