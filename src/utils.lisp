(defpackage #:utils
  (:use #:cl)
  (:export #:defclass-easy
	   #:with-gensyms
	   #:define-packet))

(in-package #:utils)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun slot->class-slot (slot)
  `(,(first slot) :initarg ,(intern (symbol-name (first slot)) "KEYWORD")
	  :initform NIL
	  :accessor ,(first slot)))

(defmacro defclass-easy (class-name supper-class slots &optional (doc "None") )
  "macro for make new class easier(don't support parents yet)"
  `(defclass ,class-name ,supper-class
     ,(mapcar #'slot->class-slot slots)
     (:documentation ,doc)))


(defgeneric data-to-byte (data byte)
  (:documentation "doc"))


(defmethod data-to-byte (data (byte (eql 'u1)))
  (list data))

(defmethod data-to-byte (data (byte (eql 'u2)))
  (list (ldb (byte 8 0) data)
	(ldb (byte 8 8) data)))

(defun slot->byte (slot)
  `(data-to-byte ,(first slot) ,(second slot)))

(defmacro define-packet (packet-name slots)
  (with-gensyms (objvar)
    `(progn
       (defclass-easy ,packet-name () ,slots)

       (defmethod obj-to-data ((,objvar ,packet-name))
	 (with-slots ,(mapcar #'first slots) ,objvar
	   (append ,@(mapcar #'slot->byte slots)))))))
