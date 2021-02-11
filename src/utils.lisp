(defpackage #:utils
  (:use #:cl
	#:ximproth)
  (:export #:-clx-xim-read-frame-
	   #:byte-to-data
	   #:clx-proto-frame-opcode
	   #:defclass-easy
	   #:data-to-byte
	   #:define-class-easy
	   #:define-packet
	   #:get-keyword
	   #:list->vector
	   #:load-bytes
	   #:obj-to-data
	   #:pad-2
	   #:pad-4
	   #:size-packet
	   #:strings-bytes
	   #:s-strings-bytes
	   #:with-gensyms
	   #:align-to
	   #:align-2
	   #:align-4
	   #:align-s-2
	   #:align-s-4))

(in-package #:utils)

(defun list->vector (list)
  (make-array (length list) :initial-contents (reverse list) :fill-pointer (length list)))

(defun get-keyword (string)
  (if (simple-string-p string)
      (intern (string-upcase string) :keyword)
      (intern (symbol-name string) :keyword)))

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

(defun pad-to (to len)
  (if (= 0 (mod len to))
      0
      (- to (mod len to))))

(defun pad-2 (len)
  (pad-to 2 len))

(defun pad-4 (len)
  (pad-to 4 len))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defgeneric data-to-byte (data byte &key)
  (:documentation "doc"))

(defmethod data-to-byte (data (byte (eql :u1)) &key)
  (list data))

(defmethod data-to-byte (data (byte (eql :u2)) &key n-data)
  (format t "~%~A~%" n-data)
  (when n-data
    (setf data n-data))
  (list (ldb (byte 8 0) data)
	(ldb (byte 8 8) data)))


(defmethod data-to-byte (data (byte (eql :bytes)) &key length)
  (let ((list NIL))
    (dotimes (c length)
      (push 0 list))
    list))

(defmethod data-to-byte (data (byte (eql :pads)) &key length)
  (let ((list NIL))
    (dotimes (c length)
      (push 0 list))
    list))

(defmethod data-to-byte (data (byte (eql :s-string)) &key);;single string
  (coerce (flexi-streams:string-to-octets data) 'list))

(defmethod data-to-byte (data (byte (eql :s-strings)) &key);;multiple STR
  (let ((result NIL))
    (dolist (item data)
      (setf result (cons (length item)
			 (append
			  (coerce (flexi-streams:string-to-octets item) 'list)
			  result))))
    result))

(defmethod data-to-byte (data (byte (eql :strings)) &key length)
  (labels ((s->b (s)
	     (append (data-to-byte (length s) :u2)
		   (coerce (flexi-streams:string-to-octets s) 'list)))
	   (string-to-byte (strings bytes)
	     (if strings
		 (string-to-byte (cdr strings) (append (s->b (car strings)) bytes))
		 (return-from data-to-byte bytes))))
    (string-to-byte data NIL)))

(defun load-bytes (length bytes)
  (let ((data 0))
    (dotimes (count length)
      (setf (ldb (byte 8 (* 8 count)) data) (vector-pop bytes)))
    data))

(defgeneric obj-to-data (obj)
  (:documentation "convet an obj to data"))
(defgeneric size-frame (obj)
  (:documentation "doc"))

(defgeneric clx-proto-frame-opcode (obj))

(defgeneric -clx-xim-read-frame- (data obj &key))

(defun easy-slot->class-slot (slot)
  (if (listp slot)
      slot
      `(,slot :initarg ,(intern (symbol-name slot) "KEYWORD")
	      :initform NIL
	      :accessor ,slot)))

(defmacro define-class-easy (class-name supperclasses slots)
  `(defclass ,class-name ,supperclasses
     ,(mapcar #'easy-slot->class-slot slots)))

(defun packet-slot->class-slot (slot)
  `(,(first slot) :initarg ,(intern (symbol-name (first slot)) "KEYWORD")
	  :initform NIL
	  :accessor ,(first slot)))

(defun packet-slot->byte (slot)
  `(data-to-byte ,@slot))

(defun byte->packet-slot (slot byte)
  `(setf ,(car slot) (byte-to-data ,(second slot) ,byte)
	 ,byte (pop byte)))

;; (defmacro define-packet (packet-name ;; static-size-p
;; 			 slots
;; 			 &key size-packet opcode)
;;   (with-gensyms (objvar datavar)
;;     `(progn
;;        (defclass ,packet-name NIL
;; 	 ,(mapcar #'packet-slot->class-slot slots))

;;        (defmethod obj-to-data ((,objvar ,packet-name))
;; 	 (with-slots ,(mapcar #'first slots) ,objvar
;; 	   (append ,@(mapcar #'packet-slot->byte slots))))

;;        ;; (defmethod static-size-p ((,objvar ,packet-name))
;;        ;; 	 ,static-size-p)


;;        (defmethod size-packet ((,objvar ,packet-name))
;; 	 (with-slots ,(mapcar #'first slots) ,objvar
;; 	   ,size-packet))

;;        (defmethod clx-proto-frame-opcode ((,objvar ,packet-name))
;; 	 ,opcode)
;;        (defmethod data-to-obj ((,objvar ,packet-name) ,datavar)
;; 	 (with-slots ,(mapcar #'first slots) ,objvar
;; 	   ,@data-to-obj
;; 	   )))))

(defun strings-bytes (strings)
  (labels ((s-length (strings len)
	     (if strings
		 (s-length (cdr strings) (+ len
					    (align-s-4 (+ 2 (length (car strings)))
						       NIL)))
		 (return-from strings-bytes len))))
    (s-length strings 0)))

(defun s-strings-bytes (strings)
  (labels ((s-length (strings len)
	     (if strings
		 (s-length (cdr strings) (+ len
					    1
					    (length (car strings))))
		 (return-from s-strings-bytes len))))
    (s-length strings 0)))

(defmethod byte-to-data ((type (eql :bytes)) data byte &key length)
  (if (= 0 length)
      NIL
      (load-bytes length byte)))

(defun read-byte-n-seq (byte n)
  (if (= 0 n)
      NIL
      (cons (vector-pop byte) (read-byte-n-seq byte (1- n)))))

(defmethod -clx-xim-read-frame- (byte (type (eql :s-string)) &key length)
  (flexi-streams:octets-to-string (read-byte-n-seq byte length)))

(defmethod -clx-xim-read-frame- (byte (type (eql :u1)) &key)
  (load-bytes 1 byte))

(defmethod -clx-xim-read-frame- (byte (type (eql :u2)) &key)
  (load-bytes 2 byte))

(defmethod -clx-xim-read-frame- (byte (type (eql :u4))  &key)
  (load-bytes 4 byte))

(defmethod -clx-xim-read-frame- (byte (type (eql :bytes)) &key length)
  (load-bytes length byte))

;; (defmethod -clx-xim-read-frame- :around (byte (type (eql :clx-im-ximattr-fr)) &key bytes)
;;   (let ((frames NIL))
;;     (labels ((read-frame ()
;; 	       (push (call-next-method) frames)
;; 	       (setf bytes (- bytes (size-packet (car frames))))
;; 	       (when (> bytes 0)
;; 		 (read-frame))))
;;       (read-frame))
;;     frames))

;; (defmethod -clx-xim-read-frame- :around (byte (type (eql :clx-im-xicattr-fr)) &key bytes)
;;   (let ((frames NIL))
;;     (labels ((read-frame ()
;; 	       (push (call-next-method) frames)
;; 	       (setf bytes (- bytes (size-packet (car frames))))
;; 	       (when (> bytes 0)
;; 		 (read-frame))))
;;       (read-frame))
;;     frames))




(defmacro define-read-around (obj-type)
  (with-gensyms (framevar)
    `(defmethod -clx-xim-read-frame- :around (byte (type (eql ,obj-type)) &key bytes)
       (let ((,framevar NIL))
	 (labels ((read-frame ()
		    (format t "bytes: ~A~%" bytes)
		    (push (call-next-method) ,framevar)
		    (setf bytes (- bytes 4 (size-packet (car ,framevar))))
		    (when (> bytes 0)
		      (read-frame))
		    ))
	   (read-frame))
	 ,framevar))))

(define-read-around :clx-im-ximattr-fr)
(define-read-around :clx-im-xicattr-fr)
(define-read-around :clx-im-ext-fr)



;; (defmethod -clx-xim-read-frame- :around (byte (type (eql :clx-im-ext-fr)) &key bytes)
;;   (let ((frames NIL))
;;     (labels ((read-frame ())
;; 	     (push (call-next-method frames) frames)
;; 	     (sef bytes (- bytes (size-packet (car frames))))
;; 	     (when (> bytes 0)
;; 	       (read-frame))))
;;     (read-frame))
;;   frames))

;; (defmethod -clx-xim-read-frame- (byte (type (eql :clx-im-xicattr-fr)) &key)
;;    (let ((obj (make-instance 'clx-im-xicattr-fr)))
;;      (with-slots (attribute-id type-of-value length-of-ic-attribute ic-bytearry pad) obj
;;        (setf attribute-id (-clx-xim-read-frame- byte :u2))
;;        (format t "~%attribute-id: ~A~%" attribute-id)
;;        (setf type-of-value (-clx-xim-read-frame- byte :u2))
;;        (format t "~%type-of-value: ~A~%" type-of-value)
;;        (setf length-of-ic-attribute (-clx-xim-read-frame- byte :u2))
;;        (format t "~%length-of-ic-attribute: ~A~%" length-of-ic-attribute)
;;        (setf ic-bytearry (-clx-xim-read-frame- byte :s-string :length length-of-ic-attribute))
;;        (format t "~%ic-bytearry: ~A~%" ic-bytearry)
;;        (setf pad (-clx-xim-read-frame- byte :bytes :length (pad-4 length-of-ic-attribute))))
;;      (format t "~%packet-size: ~A~%" (size-packet obj))
;;      obj))

;; (defmethod -clx-xim-read-frame- :around (byte (type (eql :clx-im-xicattr-fr)) &key bytes)
;;   (let ((frames NIL))
;;     (labels ((read-frame ()
;; 	       (format t "byte:~A~%" byte)
;; 	       (push (call-next-method) frames)
;; 	       ;; (format t "~%bytes: ~A~%" bytes)
;; 	       (format t "~%size: ~A~%" (size-packet (car frames)))
;; 	       (format t "~%ic-bytearry: ~A~%" (reverse (clx-xim::ic-bytearry (car frames))))
;; 	       ;; (setf bytes (- bytes (size-packet (car frames))))
;; 	       ;; (format t "~%bytes: ~A~%" bytes)
;; 	       ;; (format t "byte:~A~%" byte)
;; 	       (when (> (length byte) 0)
;; 		 (read-frame))))
;;       (read-frame))
;;     frames))

(defmacro define-packet (packet-name ;; static-size-p
			 slots
			 &key size-packet opcode)
  (with-gensyms (objvar typevar datavar)
    `(progn
       (defclass ,packet-name NIL
	 ,(mapcar #'packet-slot->class-slot slots))

       (defmethod obj-to-data ((,objvar ,packet-name))
	 (with-slots ,(mapcar #'first slots) ,objvar
	   (append ,@(mapcar #'packet-slot->byte slots))))

       ;; (defmethod static-size-p ((,objvar ,packet-name))
       ;; 	 ,static-size-p)

       (defmethod size-packet ((,objvar ,packet-name))
	 ,(if size-packet
	      `(with-slots ,(mapcar #'first slots) ,objvar
		,size-packet)
	      "no size-packet defined"))

       (defmethod clx-proto-frame-opcode ((,objvar ,packet-name))
	 ,(if opcode
	      opcode
	      :opcode-not-set))

       (defmethod -clx-xim-read-frame- (,datavar (,typevar (eql ,(get-keyword packet-name))) &key)
	 (let ((,objvar (make-instance ',packet-name)))
	   (with-slots ,(mapcar #'first slots) ,objvar
	     ,@(mapcar #'(lambda (slot)
			   (data->slot slot datavar))
		       slots))
	   ,objvar)))))

(defun data->slot (slot data)
  `(setf ,(first slot) (-clx-xim-read-frame- ,data ,@(cdr slot))))
