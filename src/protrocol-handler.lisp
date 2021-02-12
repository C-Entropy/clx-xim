(in-package #:clx-xim)

(defgeneric -clx-xim-handle-message- (clx-xim header data type)
  (:documentation "doc"))

(defmethod -clx-xim-handle-message- (clx-xim header data (type (eql *clx-xim-connect-reply*)))
  ;; (format t "*clx-xim-connect-reply* :~A~%" *clx-xim-connect-reply*)
  ;; (unless (eq (open-state clx-xim) :xim-open-wait-open-reply)
  ;;   (return-from -clx-xim-handle-message- NIL))
  )

(defun make-default-ext ()
  "XIM_EXT_MOVE"
  ;; (make-instance 'clx-im-ext
  ;; 		 :name "XIM_EXT_MOVE"
  ;; 		 :major-opcode *clx-xim-extension*
  ;; 		 :minor-opcode *clx-xim-ext-move*)
  )

(defun -clx-xim-send-query-extension (clx-xim)
  (let ((query-ext (make-instance 'clx-im-query-extension-fr
				  :input-method-id (connect-id clx-xim)
				  :ext (list (make-default-ext)))))
    (-clx-xim-send-frame- clx-xim query-ext)
    (setf (open-state clx-xim) :xim-open-wait-extension-reply)))

(defun -clx-xim-send-encoding-negotiation- (clx-xim)
  (let ((encoding-negotiation (make-instance 'clx-im-encoding-negotiation-fr
					     :input-method-id (connect-id clx-xim)
					     :encodings '("COMPOUND_TEXT"))))
    (format t "-clx-xim-send-encoding-negotiation-: ~A~%" (obj-to-data encoding-negotiation))
    (-clx-xim-send-frame- clx-xim encoding-negotiation))
  (setf (open-state clx-xim) :xim-open-wait-encoding-reply))

(defmethod -clx-xim-handle-message- (clx-xim header data (type (eql *clx-xim-open-reply*)))
  (unless (eq (open-state clx-xim) :xim-open-wait-open-reply)
    (return-from -clx-xim-handle-message- NIL))
  (format t "*clx-xim-open-reply* :~A~%" *clx-xim-open-reply*)
  ;; (format t "~A" data)
  (print (length data))
  (let ((frame (-clx-xim-read-frame- data :clx-im-open-reply-fr))
	(imattr-htable (imattr clx-xim))
	(icattr-htable (icattr clx-xim)))
    (dolist (item (im-ximattr frame))
      (setf (gethash (im-attribute item) imattr-htable) item))
    (dolist (item (ic-ximattr frame))
      (setf (gethash (ic-attribute item) icattr-htable) item))
    (setf (connect-id clx-xim) (input-method-id frame)
	  (imattr clx-xim) imattr-htable
	  (icattr clx-xim) icattr-htable)
    (-clx-xim-send-query-extension clx-xim)))

(defmethod -clx-xim-handle-message- (clx-xim header data (type (eql *clx-xim-query-extension-reply*)))
  (unless (eq (open-state clx-xim) :xim-open-wait-extension-reply)
    (return-from -clx-xim-handle-message- NIL))
  (format t "*clx-xim-query-extension-reply* :~A~%" *clx-xim-query-extension-reply*)
  (let ((frame (-clx-xim-read-frame- data :clx-im-query-extension-reply-fr)))
    (dolist (item (ext frame))
      (push (cons (extension-major-opcode item) (extension-minor-opcode item))
	    (extensions clx-xim))))
  (-clx-xim-send-encoding-negotiation- clx-xim))

(defmethod -clx-xim-handle-message- (clx-xim header data (type (eql *clx-xim-encoding-negotiation-reply*)))
  (unless (eq (open-state clx-xim) :xim-open-wait-encoding-reply)
    (return-from -clx-xim-handle-message- NIL))
  (let ((frame (-clx-xim-read-frame- data :clx-im-encoding-negotiation-reply-fr)))
    (when (and (eq (input-method-id frame)
		  (connect-id clx-xim))
		 (eq 0 (index frame)))
      (setf (open-state clx-xim) :xim-open-done)
      (when (callback (connect-state clx-xim))
	(funcall (callback (connect-state clx-xim)) clx-xim (user-data (connect-state clx-xim))))
      (-clx-change-event-mask- (accept-win clx-xim) :structure-notify NIL))))
