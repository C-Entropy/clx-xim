(in-package #:clx-xim)

(defgeneric -clx-xim-handle-message- (clx-xim hdr data type)
  (:documentation "doc"))

(defmethod -clx-xim-handle-message- (clx-xim hdr data (type (eql *clx-xim-connect-reply*)))
  (format t "*clx-xim-connect-reply* :~A~%" *clx-xim-connect-reply*)
  ;; (unless (eq (open-state clx-xim) :xim-open-wait-open-reply)
  ;;   (return-from -clx-xim-handle-message- NIL))
  )

(defun make-default-ext ()
  (make-instance 'clx-im-ext
		 :name "XIM_EXT_MOVE"
		 :major-opcode
		 :minor-opcode ))

(defun -clx-xim-send-query-extension (clx-xim)
  (let ((query-ext (make-instance 'clx-im-query-extension
				  :input-method-id (connect-id clx-xim)
				  :ext (make-default-ext)))
	)
    ))


(defmethod -clx-xim-handle-message- (clx-xim hdr data (type (eql *clx-xim-open-reply*)))
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
