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
     root-window
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
		 :root-window (screen-root screen)
		 :server-name (clx-xim-make-im-name (or imname
						       (getenv "XMODIFIERS")))
		 :connect-state (make-instance 'connect-state
					       :state-phase :xim-connect-fail)
		 :init NIL
		 :queue (make-array 5 :fill-pointer 0
				      :adjustable t)
		 :byte-order 1;;waiting
		 ))

(defun clx-xim-set-im-callback (clx-xim callbacks user-data)
  (setf (im-callback clx-xim) callbacks)
  (setf (user-data clx-xim) user-data))

(defun clx-xim-set-log-handler (clx-xim logger)
  (setf (logger clx-xim) logger))

(defun -clx-im-init-atoms- (display atom-names)
  (mapc (lambda (atom-name)
	  (intern-atom display atom-name))
	atom-names))

(defun -clx-xim-init- (clx-xim)
  (when (init clx-xim)
    (return-from -clx-xim-init- T))
  (setf (atoms clx-xim) (-clx-im-init-atoms- (display clx-xim)
			       '(:xim_servers
				 :locales
				 :transport
				 :_xim_protocol
				 :_xim_xconnect)))
  (setf (screen clx-xim) (display-default-screen (display clx-xim)))
  (setf (window clx-xim) (create-window :parent (screen-root (screen clx-xim))
					:x 10 :y 10
					:width 10 :height 10))
  ;; (when (not (screen clx-xim))
  ;;   ;; (or (not (screen clx-xim))
  ;;   ;;     (not (default-screen clx-xim)))
  ;;   (return-from -clx-xim-init- NIL))
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
  "get XIM_SERVERS from root-window, and trans them into aomt-name"

  (get-property (root-window clx-xim)
		:xim_servers
		:transform #'(lambda (atom-id)
			       (atom-name (display clx-xim) atom-id))))

(defun -clx-xim-preconnect-im- (clx-xim event)
  (print "-clx-xim-preconnect-im-")
  ;; (print (state-phase (connect-state clx-xim)))
  (block block-check
    (case (state-phase (connect-state clx-xim))
      (:xim_connect_check_server
       (when (eq (length (server-atoms clx-xim))
		 (index (check-server (connect-state clx-xim))))
	 (setf (state-phase (connect-state clx-xim)) :xim_connect_fail)
	 (return-from block-check))
       (case (subphase (check-server (connect-state clx-xim)))

	 (:xim_connect_check_server_prepare
	  (print ":xim_connect_check_server_prepare")
	  (setf (subphase (check-server (connect-state clx-xim)))
		:xim_connect_check_server_locale)
	  ;; (if (-clx-xim-check-server-preper- clx-xim)
	  ;;     (setf (subphase (check-server (connect-state clx-xim)))
	  ;; 	    :xim_connect_check_locale)
	  ;;     (progn (-check-next-server- clx-xim)
	  ;; 	     (return-from block-check)))
	  )
	 (:xim_connect_check_server_locale

	  (print ":xim_connect_check_server_locale")
	  (setf (subphase (check-server (connect-state clx-xim)))
		:xim_connect_check_server_transport))
	 (:xim_connect_check_server_transport
	  ;; (-clx-xim-check-server-transport- clx-xim)
	  (print ":xim_connect_check_server_transport")
	  (setf (subphase (check-server (connect-state clx-xim)))
		:xim_connect_check_server_transport_wait))
	 (:xim_connect_check_server_transport_wait
	  (print ":xim_connect_check_server_transport_wait")

	  (setf (state-phase (connect-state clx-xim))
		:wait-to-complete)
	  (setf (subphase (check-server (connect-state clx-xim)))
		:wait-to-complete)
	  ;; (case (-clx-xim-check-server-tranport-wait- clx-xim event)
	  ;;   (:action_accept
	  ;;    (setf event NIL
	  ;; 	   (state-phase (connect-state clx-xim)) :xim_connect_connect
	  ;; 	   (subphase (connect (connect-state clx-xim))) :xim_connect_connect_prepare)
	  ;;    (return-from -clx-xim-preconnect-im-))
	  ;;   (:action_failed
	  ;;    (setf event NIL)
	  ;;    (-chech-next-server clx-xim)
	  ;;    (return-from -clx-xim-preconnect-im-))
	  ;;   (:action_yield
	  ;;    (return-from -clx-xim-preconnect-im-)))
	  )))
      ;; (:xim_connect_connect
      ;; ;;  (case (subphase (connect (connect-state clx-xim)))
      ;; ;; 	 (:xim_connect_connect_prepare
      ;; ;; 	  (-clx-xim-connect-prepare- clx-xim)
      ;; ;; 	  (setf (subphase (connect (connect-state clx-xim))) :xim_connect_connect_wait))
      ;; ;; 	 (:xim_connect_connect_wait
      ;; ;; 	  (case (-clx-xim-connect-connect-wait clx-xim event)
      ;; ;; 	    (:accept_accept
      ;; ;; 	     (setf event NIL
      ;; ;; 		   (subphase (connect (connect-state clx-xim))) :xim_connect_wait_reply)
      ;; ;; 	     (return-from -clx-xim-preconnect-im-))
      ;; ;; 	    (:accept_failed
      ;; ;; 	     (setf event NIL
      ;; ;; 		   (state-phase (connect-state clx-xim)) :xim_connect_fail)
      ;; ;; 	     (return-from -clx-xim-preconnect-im-)
      ;; ;; 	     (:accept_yield
      ;; ;; 	      (return-from -clx-xim-preconnect-im-))))
      ;; ;; 	  (:xim_connect_connect_wait_reply
      ;; ;; 	   (case (-clx-xim-connect-wait-reply clx-xim event)
      ;; ;; 	     (:action_accept
      ;; ;; 	      (setf event NIL
      ;; ;; 		    (state-phase (connect-state clx-xim)) :xim_connect_done)
      ;; ;; 	      (return-from -clx-xim-preconnect-im-))
      ;; ;; 	     (:action_failed
      ;; ;; 	      (setf event NIL
      ;; ;; 		    (state-phase (connect-state clx-xim)) :xim_connect_fail)
      ;; ;; 	      (return-from -clx-xim-preconnect-im-)
      ;; ;; 	      (:action_yield
      ;; ;; 	       (return-from -clx-xim-preconnect-im-)))))

      ;; ;; 	  (otherwise (return-from -clx-xim-preconnect-im-)))))
      (otherwise (return-from -clx-xim-preconnect-im-))))
  (-clx-xim-preconnect-im- clx-xim event)
  )

(defun -clx-xim-open- (clx-xim)
  (setf (open-state clx-xim) :xim-open-invalid
	(state-phase (connect-state clx-xim)) :xim-connect-fail)

  (unless (-clx-xim-init- clx-xim)
    (return-from -clx-xim-open- NIL))

  (when (auto-connect clx-xim)
    (-clx-change-event-mask- (root-window clx-xim) :property-change NIL))

  (unless (setf (server-atoms clx-xim) (-clx-xim-get-servers- clx-xim))
    (return-from -clx-xim-open- NIL))

  (setf (state-phase (connect-state clx-xim)) :xim_connect_check_server)
  ((lambda (check-server)
     (setf (index check-server) 0)
     (setf (requestor-window check-server) 0)
     (setf (window check-server) 0)
     (setf (subphase check-server) :xim_connect_check_server_prepare))
   (check-server (connect-state clx-xim)))
  (-clx-xim-preconnect-im- clx-xim NIL))


(defun clx-xim-open (clx-xim clx-xim-open-callback auto-connect user-data)
  (funcall (lambda (connect-state)
	     (setf (callback connect-state) clx-xim-open-callback
		   (user-data connect-state) user-data))
	   (connect-state clx-xim))
  (setf (auto-connect clx-xim) auto-connect)
  (-clx-xim-open- clx-xim))
