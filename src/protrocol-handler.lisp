(in-package #:clx-xim)

(defgeneric -clx-xim-handle-message- (clx-xim header data type)
  (:documentation "doc"))

(defmethod -clx-xim-handle-message- (clx-xim header data (type (eql *clx-xim-connect-reply*)))
 (format t "*clx-xim-connect-reply* ~%")
  (unless (= (major-opcode header) *clx-xim-connect-reply*)
    (return-from -clx-xim-handle-message- NIL))

    (let ((reply-frame (-clx-xim-read-frame- data :clx-im-connect-reply-fr)))
      (-clx-xim-send-open- clx-xim)
      (setf (state-phase (connect-state clx-xim)) :xim-connect-done)))

(defun make-default-ext ()
  "XIM_EXT_MOVE"
  ;; (make-instance 'clx-im-ext
  ;; 		 :name "XIM_EXT_MOVE"
  ;; 		 :major-opcode *clx-xim-extension*
  ;; 		 :minor-opcode *clx-xim-ext-move*)
  )

(defun -clx-xim-send-query-extension- (clx-xim)
  (let ((query-ext (make-instance 'clx-im-query-extension-fr
				  :input-method-id (connect-id clx-xim)
				  :ext (list (make-default-ext)))))
    (-clx-xim-send-frame- clx-xim query-ext)
    (setf (open-state clx-xim) :xim-open-wait-extension-reply)))

(defun -clx-xim-send-encoding-negotiation- (clx-xim)
  (let ((encoding-negotiation (make-instance 'clx-im-encoding-negotiation-fr
					     :input-method-id (connect-id clx-xim)
					     :encodings '("COMPOUND_TEXT"))))
    (-clx-xim-send-frame- clx-xim encoding-negotiation))
  (setf (open-state clx-xim) :xim-open-wait-encoding-reply))

(defmethod -clx-xim-handle-message- (clx-xim header data (type (eql *clx-xim-open-reply*)))
  (format t "handling *clx-xim-open-reply* ~%")
  (unless (eq (open-state clx-xim) :xim-open-wait-open-reply)
    (return-from -clx-xim-handle-message- NIL))
  (let ((frame (-clx-xim-read-frame- data :clx-im-open-reply-fr))
	(imattr-htable (imattr clx-xim))
	(icattr-htable (icattr clx-xim)))
    ;; (format t "handling *clx-xim-open-reply* :~A~%" *clx-xim-open-reply*)
    (dolist (item (im-ximattr frame))
      (setf (gethash (im-attribute item) imattr-htable) item))
    (dolist (item (ic-ximattr frame))
      (setf (gethash (ic-attribute item) icattr-htable) item))
    (setf (connect-id clx-xim) (input-method-id frame)
	  (imattr clx-xim) imattr-htable
	  (icattr clx-xim) icattr-htable)
    (-clx-xim-send-query-extension- clx-xim))
  ;; (format t "handling *clx-xim-open-reply* :~A~%" *clx-xim-open-reply*)
  )

(defmethod -clx-xim-handle-message- (clx-xim header data (type (eql *clx-xim-query-extension-reply*)))
  (format t "handling *clx-xim-query-extension-reply* ~%")
  (unless (eq (open-state clx-xim) :xim-open-wait-extension-reply)
    (return-from -clx-xim-handle-message- NIL))
  (let ((frame (-clx-xim-read-frame- data :clx-im-query-extension-reply-fr)))
    (dolist (item (ext frame))
      (push (cons (extension-major-opcode item) (extension-minor-opcode item))
	    (extensions clx-xim))))
  (-clx-xim-send-encoding-negotiation- clx-xim))

(defmethod -clx-xim-handle-message- (clx-xim header data (type (eql *clx-xim-encoding-negotiation-reply*)))
  (format t "handling *clx-xim-encoding-negotiation-reply* ~%")
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

(defmethod -clx-xim-handle-message- (clx-xim header data (type (eql *clx-xim-create-ic-reply*)))
  (format t "handling *clx-xim-create-ic-reply* ~%")
  (let ((frame (-clx-xim-read-frame- data :clx-im-create-ic-reply-fr)))
    (unless (and (current clx-xim)
		 (eq (major-code (current clx-xim)) *clx-xim-create-ic*)
		 (eq (connect-id clx-xim) (input-method-id frame)))
      (return-from -clx-xim-handle-message- NIL))
    (let ((request (current clx-xim)))
      (setf (current clx-xim) NIL)
      (funcall (callback request) clx-xim (input-context-id frame) (user-data request)))))

(defmethod -clx-xim-handle-message- (clx-xim header data (type (eql *clx-xim-set-event-mask*)))
  (format t "handling *clx-xim-set-event-mask* ~%")
  (let ((frame (-clx-xim-read-frame- data :clx-im-set-event-mask-fr)))
    (unless (and (eq (connect-id clx-xim) (input-method-id frame))
		 (assoc :set-event-mask (im-callback clx-xim)))
      (return-from -clx-xim-handle-message- NIL))
    (funcall (cdr (assoc :set-event-mask (im-callback clx-xim)))
	       clx-xim (input-context-id frame) (forward-event-mask frame) (synchronous-event-mask frame) (user-data clx-xim))))


(defmethod -clx-xim-handle-message- (clx-xim header data (type (eql *clx-xim-forward-event*)))
  (format t "~%handling *clx-xim-forward-event* ~%")
  (let ((frame (-clx-xim-read-frame- data :clx-im-forward-event-fr)))
    (when (or (< (header-bytes header) 10);;10 = (/ (+ (size-packet frame) (size key press)) 4)
	       (not (eq (connect-id clx-xim)
			(input-method-id frame))))
      (return-from -clx-xim-handle-message- NIL))
    (let ((key-event (-clx-xim-read-frame- data :clx-im-key-press-event-fr)))
      (when (assoc :forward-event (im-callback clx-xim))
	;; (print (cdr (assoc :forward-event (im-callback clx-xim))))
	(funcall (cdr (assoc :forward-event (im-callback clx-xim)))
		 clx-xim (input-context-id frame) (code key-event) (state key-event) (response-type key-event) (user-data clx-xim)))
      (when (eq (flag frame) *clx-xim-synchronous*)
	(-clx-xim-sync- clx-xim (input-context-id frame))))))



(defmethod -clx-xim-handle-message- (clx-xim header data (type (eql *clx-xim-register-triggerkeys*)))
  (format t "~%handling *clx-xim-register-triggerkeys* ~%")
  (let ((frame (-clx-xim-read-frame- data :clx-im-register-triggerkeys-fr)))
    (setf (onkeys clx-xim) (on-key frame)
	  (offkeys clx-xim) (off-key frame))))

(defmethod -clx-xim-handle-message- (clx-xim header data (type (eql *clx-xim-error*)))
  (format t "~%handling *clx-xim-error* ~%"))
