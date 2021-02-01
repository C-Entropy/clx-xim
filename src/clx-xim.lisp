(defpackage clx-xim
  (:use #:cl
	#:utils
	#:xlib)
  (:import-from #:uiop #:getenv)
  (:export #:make-clx-xim
	   #:clx-xim-open
	   #:clx-xim-set-im-callback
	   #:clx-xim-set-log-handler
	   ;;class clx-xim itself
	   #:display
	   #:logger
	   #:im-callback))
(in-package #:clx-xim)

(defclass-easy clx-xim ()
    (;;basic data which should always be valid
     display
     server-name
     window
     im-callback
     user-data
     ;;some global data
     ;; sequence
     byte-order
     ;;set by -clx-xim-init
     screen
     default-screen
     atoms; ;;add one for SERVER-NAME
     init
     ;;set by -clx-xim-get-servers
     server-atoms
     n-server-atoms
     ;;used by -clx-xim-check-server / -clx-xim-connect
     connect-state
     ;;-clx-xim-check-server
     trans-addr
     im-window
     ;;and also server-atom
     im-client-window
     ;;-clx-xim-connect-wait
     major-code
     minor-code
     accept-win
     ;;xim open
     open-state
     connect-id
     imattr
     icattr
     extensions
     onKeys
     offKeys
     ;;request
     current
     queue
     nExtensions
     auto-connect
     ;;Indicate whether we need a recheck on the new server.
     recheck
     yield-recheck
     ;;some ic values
     client-window
     focus-window
     logger))

(defclass-easy connect-state ()
    (state-phase
     callback
     user-data
     (check-server :initform (make-instance 'check-server)
		   :accessor check-server)))

(defclass-easy check-server ()
    (index
     subphase
     window
     requestor-window))

(defun clx-xim-make-im-name (im-name)
  "make im name using im-name, cutting down '@im=', then return the left part"
  (funcall (lambda (length)
	     (cond ((and (> (length im-name) length)
			 (string= (subseq im-name 0 length) "@im="))
		    (subseq im-name length))
		   (t
		    NIL)))
	   (length "@im=")))

(defun make-clx-xim (display screen
		     &key imname)
  "create a clx-xim and return it."
  ;; (or imname
  ;;   (getenv "XMODIFIERS"))
  (make-instance 'clx-xim
		 :display display
		 :default-screen screen
		 :server-name (clx-xim-make-im-name (or imname
						       (getenv "XMODIFIERS")))
		 :connect-state (make-instance 'connect-state
					       :state-phase :xim-connect-fail)
		 :queue (make-array 5 :fill-pointer 0
				      :adjustable t)
		 :byte-order 1;;waiting
		 ))

(defun clx-xim-set-im-callback (clx-xim callbacks user-data)
  (setf (im-callback clx-xim) callbacks)
  (setf (user-data clx-xim) user-data))

(defun clx-xim-set-log-handler (clx-xim logger)
  (setf (logger clx-xim) logger))

(defun -clx-im-init-atoms- (display atom-names atoms)
  (mapc (lambda (atom-name)
	  (push (cons atom-name
		      (intern-atom display atom-name))
		atoms))
	atom-names))

(defun -clx-xim-init- (clx-xim)
  (when (init clx-xim)
    (return-from -clx-xim-init- T))

  (unless (-clx-im-init-atoms- (display clx-xim)
			       '(:xim-servers
				 :locales
				 :transport
				 :_xim_protocol
				 :_xim_xconnect)
			       (atoms clx-xim))
    (return-from -clx-xim-init- NIL))
  (setf (screen clx-xim) (display-default-screen (display clx-xim)))
  (when (or (not (screen clx-xim))
	    (not (default-screen clx-xim)))
    (return-from -clx-xim-init- NIL))
  (setf (init clx-xim) T))


(defun -clx-change-event-mask- (window mask-key remove)
  (let ((event-mask-keys (make-event-keys (window-event-mask window))))
    (when remove
      (when (find mask-key event-mask-keys)
	(setf (window-event-mask window) (remove mask-key event-mask-keys)))
      (return-from -clx-change-event-mask-))
    (unless (find mask-key event-mask-keys);;when remove is NIL, we do add
      (setf (window-event-mask window) (cons mask-key event-mask-keys)))))

(defun -clx-xim-get-servers- (clx-xim)
  (setf (server-atoms clx-xim) (get-property  (window clx-xim)
					     (cdr (assoc :xim-servers (atoms clx-xim))))))

(defun -clx-xim-preconnect-im- (clx-xim event)
  NIL)

(defun -clx-xim-open- (clx-xim)
  (setf (state-phase (connect-state clx-xim)) :xim-connect-fail)
  (setf (open-state clx-xim) :xim-open-invalid)

  (unless (-clx-xim-init- clx-xim)
    (return-from -clx-xim-open- NIL))

  (when (auto-connect clx-xim)
    (-clx-change-event-mask- (window clx-xim) :property-change NIL))

  (unless (-clx-xim-get-servers- clx-xim)
    (return-from -clx-xim-open- NIL))

  ;; (setf (state-phase (connect-state clx-xim)) :xim_connect_check_server)
  ;; ((lambda (check-server)
  ;;    (setf (index check-server) 0)
  ;;    (setf (requestor-window check-server) 0)
  ;;    (setf (window check-server) 0)
  ;;    (setf (subphase check-server) :xim_connect_check_server_prepare))
  ;;  (check-server (connect-state clx-xim)))
  ;; (-clx-xim-preconnect-im- clx-xim nNIL)
  )


(defun clx-xim-open (clx-xim clx-xim-open-callback auto-connect user-data)
  (funcall (lambda (connect-state)
	     (setf (callback connect-state) clx-xim-open-callback)
	     (setf (user-data connect-state) user-data))
	   (connect-state clx-xim))
  (setf (auto-connect clx-xim) auto-connect)
  (-clx-xim-open- clx-xim))





;; (defun clx-xim-filter-event (clx-xim event)
;;   (setf (yield-recheck clx-xim) NIL)
;;   (let ((result (get-filter-result clx-xim event)))
;;     (when (yield-recheck clx-xim)
;;       (-clx-xim-clean-up- clx-xim))
;;     result)
;;   )

;; (defun get-filter-result (clx-xim event)
;;   (or (-clx-xim-preconnect-im- (clx-xim event))
;;       (-clx-xim-filter-event- (clx-xim event))
;;       (-clx-xim-filter-destroy-window- (clx-xim event))
;;       (-clx-xim-filter-property-changed- (clx-xim event))))

;; 					;TODO: 1. -clx-xim-filter-destroy-window-
;; 					;TODO: 2. -clx-xim-filter-event-
;; 					;TODO: 3. -clx-xim-filter-property-changed-
;; 					;TODO: 4. -clx-xim-preconnect-im-

;; (defun -clx-xim-preconnect-im- (clx-xim event)
;;   (when (funcall (lambda (test)
;; 		   (and (not (eq test XIM-CONNECT-DONE))
;; 			(not (eq test XIM-CONNECT-FAIL))))
;; 		 (connect-state-phase clx-xim))
;;     (case  (connect-state-phase clx-xim)
;;       (XIM-CONNECT-CHECK-SERVER
;;        (when (eq (connect-state-check-server-subphase clx-xim) (n-server-atoms clx-xim))
;; 	 (setf (connect-state-phase clx-xim) XIM-CONNECT-FAIL)
;; 	 (-clx-xim-preconnect-im- clx-xim event)
;; 	 (return-from -clx-xim-preconnect-im-))
;;        (cond ((eq (connect-state-check-server-subphase clx-xim) XIM-CONNECT-CHECK-SERVER-PREPARE)
;; 	      (cond ((-clx-xim-check-server-prepare clx-xim)
;; 		     (setf (connect-state-check-server-subphase im) XIM-CONNECT-CHECK-SERVER-LOCALE))
;; 		    (t
;; 		     (next-server-chek clx-xim)
;; 		     (-clx-xim-preconnect-im- (clx-xim event))
;; 		     (return-from -clx-xim-preconnect-im-))))
;; 	     (case (connect-state-check-server-subphase clx-xim)
;; 	       (XIM-CONNECT-CHECK-SERVER-LOCALE
;; 		())
;; 	       (XIM-CONNECT-CHECK-SERVER-TRANSPORT
;; 		())
;; 	       (XIM-CONNECT-CHECK-SERVER-TRANSPORT-WAIT
;; 		()))))
;;       (XIM-CONNECT-CONNECT
;;        (case (connect-state-connect-subphase clx-xim)
;; 	 (XIM-CONNECT-CONNECT-PREPARE
;; 	  (-clx-xim-connect-prepare clx-xim)
;; 	  (setf (connect-state-connect-subphase clx-xim) XIM-CONNECT-CONNECT-WAIT))
;; 	 (XIM-CONNECT-CONNECT-WAIT
;; 	  ())
;; 	 (XIM-CONNECT-CONNECT-WAIT-REPLY
;; 	  (case (-clx-xim-connect-wait-reply clx-xim event)
;; 	    (ACTION-YIELD
;; 	     (return-from -clx-xim-preconnect-im-))
;; 	    (ACTION-ACCEPT
;; 	     (setf event NIL)
;; 	     (setf (connect-state-phase clx-xim) XIM-CONNECT-DONE)
;; 	     (return-from -clx-xim-preconnect-im-))
;; 	    (ACTION-FAILED
;; 	     (setf event NIL)
;; 	     (setf (connect-state-phase clx-xim) XIM-CONNECT-FAIL)
;; 	     (return-from -clx-xim-preconnect-im-)))))))
;;     (-clx-xim-preconnect-im- clx-xim event))
;;   (eq event NIL))

;; (defun -clx-xim-filter-event- (clx-xim event)
;;   (unless (eq (connect-state-phase) XIM-CONNECT-DONE)
;;     (return-from -clx-xim-filter-event- NIL))
;;   (when )
;;   )
;; (defun -clx-xim-filter-destroy-window- (clx-xim event)
;;   (cond ((not (eq (open-state clx-xim) XIM-OPEN-DONE))
;; 	 (return-from -clx-xim-filter-destroy-window- NIL))
;; 	(not (eq ( event) )
;; 	     )
;; 	))

;; (defun -clx-xim-filter-property-changed- (clx-xim event))
