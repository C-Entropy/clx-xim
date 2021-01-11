(defpackage clx-im
  (:use #:cl
	#:xlib
	)
  (:import-from #:uiop #:getenv)
  (:export make-clx-im
	   clx-im-open))
(in-package #:clx-im)



(defclass clx-im ()
  (
   (auto-connect :initarg auto-connect :accessor auto-connect)
   (byte-order :initarg :byte-order :accessor byte-order;; waiting complete
	       )
   (conn :initarg :conn :accessor conn)
   ;; (screen-id :initarg :screen-id :accessor screen-id)
   (connect-state :initarg :connect-state :accessor connect-state)
   (connect-state-phase :initarg connect-state-phase :accessor connect-state-phase)
   (connect-state-callback :initarg connect-state-callback :accessor connect-state-callback)
   (connect-state-user-data :initarg connect-state-user-data :accessor connect-state-user-data)
   (open-state :initarg open-state :accessor open-state)
   (server-name :initarg :server-name :accessor server-name)
   (yield-recheck :initarg :yield-recheck :accessor yield-recheck))
  (:documentation "Basic class CLX-IM"))

(defun clx-im-make-im-name (im-name)
  "make im name using im-name, cutting down "@im=", then return the left part"
  (funcall (lambda (length)
	     (cond ((and (> (length im-name) length)
			 (string= (subseq im-name 0 length) "@im="))
		    (subseq im-name length))
		   (t
		    NIL)))
	   (length "@im=")))


(defun make-clx-im (conn ;; screen-id
		    &optional imname)
  "create a clx-im and return it."
  ;; (or imname
  ;;   (getenv "XMODIFIERS"))
  (make-instance 'clx-im
		 :conn conn
		 :server-name (clx-im-make-im-name (or imname
						       (getenv "XMODIFIERS")))
		 ;; :screen-id screen-id
		 :connect-state-phase NIL;;(XCL-CONNECT-FAIL)
		 :byte-order 1;;waiting
		 ))

(defun clx-im-open (clx-im callback auto-connect user-data)
  ;; (print (server-name clx-im))
  (setf (connect-state-callback clx-im) callback)
  (setf (auto-connect clx-im) auto-connect)
  (setf (connect-state-user-data clx-im) user-data)
  (-clx-im-open clx-im)
  )


(defun -clx-im-open- (clx-im)
  (setf (connect-state-phase clx-im) 'XIM-CONNECT_FAIL)
  (setf (open-state clx-im) 'XIM-OPEN-INVALID)
  (unless -clx-im-init- (clx-im)
	  nil)
  (when (auto-connect clx-im)
    (-clx-change-event-mask- (conn clx-im) ())))

(defun clx-im-filter-event (clx-im event)
  (setf (yield-recheck clx-im) NIL)
  (let ((result (get-filter-result clx-im event)))
    (when (yield-recheck clx-im)
      (-clx-im-clean-up- clx-im))
    result)
  )

(defun get-filter-result (clx-im event)
  (or (-clx-im-preconnect-im- (clx-im event))
      (-clx-im-filter-event- (clx-im event))
      (-clx-im-filter_destroy-window- (clx-im event))
      (-clx-im-filter_property-changed- (clx-im event))))

					;TODO: 1. -clx-im-filter_destroy-window-
					;TODO: 2. -clx-im-filter-event-
					;TODO: 3. -clx-im-filter_property-changed-
					;TODO: 4. -clx-im-preconnect-im-

(defun -clx-im-preconnect-im- (clx-im event)
  (when (funcall (lambda (test)
		   (and (not (eq test XIM-CONNECT-DONE))
			(not (eq test XIM-CONNECT-FAIL))))
		 (connect-state-phase clx-im))
    (case  (connect-state-phase clx-im)
      (XIM-CONNECT-CHECK-SERVER
       (when (eq (connect-state-check-server-subphase clx-im) (n-server-atoms clx-im))
	 (setf (connect-state-phase clx-im) XIM-CONNECT-FAIL)
	 (-clx-im-preconnect-im- clx-im event)
	 (return-from -clx-im-preconnect-im-))
       (cond ((eq (connect-state-check-server-subphase clx-im) XIM-CONNECT-CHECK-SERVER-PREPARE)
	      (cond ((-clx-im-check-server-prepare clx-im)
		     (setf (connect-state-check-server-subphase im) XIM-CONNECT-CHECK-SERVER-LOCALE))
		    (t
		     (next-server-chek clx-im)
		     (-clx-im-preconnect-im- (clx-im event))
		     (return-from -clx-im-preconnect-im-))))
	     (case (connect-state-check-server-subphase clx-im)
	       (XIM-CONNECT-CHECK-SERVER-LOCALE
		())
	       (XIM_CONNECT_CHECK_SERVER_TRANSPORT
		())
	       (XIM_CONNECT_CHECK_SERVER_TRANSPORT_WAIT
		()))))
      (XIM-CONNECT-CONNECT
       (case (connect-state-connect-subphase clx-im)
	 (XIM-CONNECT-CONNECT-PREPARE
	  (-clx-im-connect-prepare clx-im)
	  (setf (connect-state-connect-subphase clx-im) XIM-CONNECT-CONNECT-WAIT))
	 (XIM-CONNECT-CONNECT-WAIT
	  ())
	 (XIM-CONNECT-CONNECT-WAIT-REPLY
	  (case (-xcb-xim-connect-wait-reply clx-im event)
	    (ACTION-YIELD
	     (return-from -clx-im-preconnect-im-))
	    (ACTION-ACCEPT
	     (setf event NIL)
	     (setf (connect-state-phase clx-im) XIM-CONNECT-DONE)
	     (return-from -clx-im-preconnect-im-))
	    (ACTION-FAILED
	     (setf event NIL)
	     (setf (connect-state-phase clx-im) XIM-CONNECT-FAIL)
	     (return-from -clx-im-preconnect-im-)))))))
    (-clx-im-preconnect-im- clx-im event))
  (eq event NIL))

(defun -clx-im-filter-event- (clx-im event)
  (unless (eq (connect-state-phase) XIM-CONNECT-DONE)
    (return-from -clx-im-filter-event- NIL))
  (when )
  )
(defun -clx-im-filter-destroy-window- (clx-im event)
  (cond ((not (eq (open-state clx-im) XIM-OPEN-DONE))
	 (return-from -clx-im-filter-destroy-window- NIL))
	(not (eq ( event) )
	     )
	))

(defun -clx-im-filter-property-changed- (clx-im event))
