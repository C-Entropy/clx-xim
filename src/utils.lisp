(defpackage #:utils
  (:use #:cl)
  (:export #:defclass-easy
	   #:data-to-byte
	   #:define-class-easy
	   #:define-packet
	   #:obj-to-data
	   #:size-packet
	   #:with-gensyms))

(in-package #:utils)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defgeneric data-to-byte (data byte)
  (:documentation "doc"))


(defmethod data-to-byte (data (byte (eql :u1)))
  (list data))

(defmethod data-to-byte (data (byte (eql :u2)))
  (list (ldb (byte 8 0) data)
	(ldb (byte 8 8) data)))

(defmethod data-to-byte (data (byte (eql :strings)))
  (list data))

(defgeneric obj-to-data (obj)
  (:documentation "convet an obj to data"))
(defgeneric size-frame (obj)
  (:documentation "doc"))

(defun easy-slot->class-slot (slot)
  (if (listp slot)
      slot
      `(,slot :initarg ,(intern (symbol-name slot) "KEYWORD")
	      :accessor ,slot)))

(defmacro define-class-easy (class-name supperclasses slots)
  `(defclass ,class-name ,supperclasses
     ,(mapcar #'easy-slot->class-slot slots)))

(defun packet-slot->class-slot (slot)
  `(,(first slot) :initarg ,(intern (symbol-name (first slot)) "KEYWORD")
	  :initform NIL
	  :accessor ,(first slot)))

(defun packet-slot->byte (slot)
  `(data-to-byte ,(first slot) ,(second slot)))

(defmacro define-packet (packet-name ;; static-size-p
			 slots
			 &key size-packet
			 )
  (with-gensyms (objvar)
    `(progn
       (defclass ,packet-name NIL
	 ,(mapcar #'packet-slot->class-slot slots))

       (defmethod obj-to-data ((,objvar ,packet-name))
	 (with-slots ,(mapcar #'first slots) ,objvar
	   (append ,@(mapcar #'packet-slot->byte slots))))

       ;; (defmethod static-size-p ((,objvar ,packet-name))
       ;; 	 ,static-size-p)


       (defmethod size-packet ((,objvar ,packet-name))
	 ,size-packet))))
