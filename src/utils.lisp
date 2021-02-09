(defpackage #:utils
  (:use #:cl
	#:ximproth)
  (:export #:byte-to-data
	   #:clx-proto-frame-opcode
	   #:defclass-easy
	   #:data-to-byte
	   #:define-class-easy
	   #:define-packet
	   #:obj-to-data
	   #:size-packet
	   #:strings-bytes
	   #:with-gensyms
	   #:align-to
	   #:align-2
	   #:align-4))

(in-package #:utils)

(defun align-to (to ptr len remain)
  (let ((diff (if (= 0 (mod len to))
		  0
		  (- to (mod len to)))))
    (when remain
      (when (< remain diff)
	(return-from align-to 0))
      (setf remain (- remain diff)))
    (+ ptr diff)))

(defun align-2 (ptr len remain)
  (align-to 2 ptr len remain))

(defun align-4 (ptr len remain)
  (align-to 4 ptr len remain))

(defun align-s-2 (s remain)
  (align-to 2 s s remain))

(defun align-s-4 (s remain)
  (align-to 4 s s remain))

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

(defgeneric byte-to-data (type byte))

(defmethod byte-to-data ((type (eql :u1)) byte)
  (first byte))

(defmethod byte-to-data ((type (eql :u2)) byte)
  (let ((data 0))
    (setf (ldb (byte 8 0) data) (pop byte))
    (setf (ldb (byte 8 8) data) (pop byte))
    data))

(defmethod data-to-byte (data (byte (eql :strings)))
  (labels ((s->b (s)
	     (append (data-to-byte (length s) :u2)
		   (coerce (flexi-streams:string-to-octets s) 'list)))
	   (string-to-byte (strings bytes)
	     (if strings
		 (string-to-byte (cdr strings) (append (s->b (car strings)) bytes))
		 (return-from data-to-byte bytes))))
    (string-to-byte data NIL)))

(defgeneric obj-to-data (obj)
  (:documentation "convet an obj to data"))
(defgeneric size-frame (obj)
  (:documentation "doc"))

(defgeneric clx-proto-frame-opcode (obj))

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

(defun byte->packet-slot (slot byte)
  `(setf ,(car slot) (byte-to-data ,(second slot) ,byte)
	 ,byte (pop byte)))

(defmacro define-packet (packet-name ;; static-size-p
			 slots
			 &key size-packet opcode)
  (with-gensyms (objvar datavar)
    `(progn
       (defclass ,packet-name NIL
	 ,(mapcar #'packet-slot->class-slot slots))

       (defmethod obj-to-data ((,objvar ,packet-name))
	 (with-slots ,(mapcar #'first slots) ,objvar
	   (append ,@(mapcar #'packet-slot->byte slots))))

       ;; (defmethod static-size-p ((,objvar ,packet-name))
       ;; 	 ,static-size-p)


       (defmethod size-packet ((,objvar ,packet-name))
	 (with-slots ,(mapcar #'first slots) ,objvar
	   ,size-packet))

       (defmethod clx-proto-frame-opcode ((,objvar ,packet-name))
	 ,opcode)

       ;; (defmethod data-to-obj ((,objvar ,packet-name) ,datavar)
       ;; 	 (with-slots ,(mapcar #'first slots) ,objvar
       ;; 	   ,@data-to-obj
       ;; 	   ))
       )))

(defun strings-bytes (strings)
  (labels ((s-length (strings len)
	     (if strings
		 (s-length (cdr strings) (align-s-4 (+ 2 len (length (car strings)))
						    NIL))
		 (return-from strings-bytes len))))
    (s-length strings 0)))
