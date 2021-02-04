(defpackage #:utils
  (:use #:cl)
  (:export #:defclass-easy))

(in-package #:utils)

(defun slot->class-slot (slot)
  (if (listp slot)
      slot
      `(,slot :initarg ,(intern (symbol-name slot) "KEYWORD")
	      :initform NIL
	      :accessor ,slot)))

(defmacro defclass-easy (class-name supper-class slots &optional (doc "None") )
  "macro for make new class easier(don't support parents yet)"
  `(defclass ,class-name ,supper-class
     ,(mapcar #'slot->class-slot slots)
     (:documentation ,doc)))
