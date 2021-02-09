(defpackage clx-xim
  (:use #:cl
	#:protrocol-handler
	#:ximproth
	#:utils
	#:xlib)
  (:import-from #:uiop #:getenv)
  (:export #:make-clx-xim
	   #:clx-xim-client-message
	   #:clx-xim-open
	   #:clx-xim-set-im-callback
	   #:clx-xim-set-log-handler
	   ;;class clx-xim itself
	   ;; #:display
	   ;; #:logger
	   #:im-callback))
(in-package #:clx-xim)


(defparameter *xim-server-category* "@server=")
(defparameter *xim-locale-category* "@locale=")
(defparameter *xim-transport-category* "@transport=")
(defparameter *clx-xim-cm-data-size* 20)
(defparameter *clx-xim-header-size* 4
  "doc")

(define-class-easy clx-xim ()
  (;;basic data which should always be valid
   display
   server-name
   window
   im-callback
   user-data
   root-window
   ;;some global data
   xim-sequence
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
   ;;-clx-xim-connect-wait-
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

(define-class-easy connect-state ()
  (state-phase
   callback
   user-data
   connect-subphase
   (check-server :initform (make-instance 'check-server)
		 :accessor check-server)))

(define-class-easy check-server ()
  (index
   subphase
   window
   requestor-window))

;; (define-packet clx-im-xpcs-fr-t
;;     ((length-of-string-in-bytes :u1)
;;      (fr-string :u1)))

(defgeneric -clx-xim-read-frame- (obj data))

(define-packet clx-im-connect-fr
    ((byte-order :u1)
     (pad :u1)
     (client-major-protocol-version :u2)
     (client-minor-protocol-version :u2)
     (protocol-size :u2)
     (protocol-items :strings))
  :opcode *clx-xim-connect*
  :size-packet
  (+ 8
     (strings-bytes protocol-items)))

(defmethod obj-to-data :before ((frame clx-im-connect-fr))
  (setf (protocol-size frame) (strings-bytes (protocol-items frame))))

;; (obj-to-data (make-instance 'clx-im-connect-fr
;; 			    :byte-order "l"
;; 			    :pad 0
;; 			    :client-major-protocol-version 0
;; 			    :client-minor-protocol-version 0
;; 			    ;; :protocol-size 0
;; 			    ;; :protocol-items '("0" "a")
;; 			    ))

(define-packet clx-im-packet-header-fr
    ((major-opcode :u1)
     (minor-opcode :u1)
     (header-bytes :u2)))

(defmethod -clx-xim-read-frame- ((obj clx-im-packet-header-fr) data)
  (setf (major-opcode obj) (byte-to-data :u1 data))
  (pop data)
  (setf (minor-opcode obj) (byte-to-data :u1 data))
  (pop data)
  (setf (header-bytes obj) (byte-to-data :u2 data))
  (pop data) (pop data)
  obj)

(define-packet clx-im-connect-reply-fr
    ((server-major-protocol-version :u2)
     (server-minor-protocol-version :u2)))

(defun clx-im-str-fr-size (string)
  (1+ (length string)))

(define-packet clx-xim-open-fr
    ((length-of-string :u1)
     (s-string :s-string))
  :opcode *clx-xim-open*
  :size-packet
  (align-s-4 (clx-im-str-fr-size s-string) NIL))

(defmethod obj-to-data :before ((frame clx-xim-open-fr))
  (setf (length-of-string frame) (length (s-string frame))))

(defmethod obj-to-data :around ((frame clx-xim-open-fr))
  (call-next-method))

(defmethod -clx-xim-read-frame- ((obj clx-im-connect-reply-fr) data)
  (setf (server-major-protocol-version obj) (byte-to-data :u2 data))
  (pop data)
  (pop data)
  (setf (server-minor-protocol-version obj) (byte-to-data :u2 data))
  obj)

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
		 :connect-id 0
		 :xim-sequence 0
		 :queue (make-array 5 :fill-pointer 0
				      :adjustable t)
		 :byte-order (if (= 1 (ldb (byte 8 0) 1))
				 (char-code #\l)
				 (char-code #\B))))

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

  (setf (atoms clx-xim)
	(mapcar #'list
		(-clx-im-init-atoms- (display clx-xim)
				     '(:xim_servers
				       :locales
				       :transport
				       :_xim_protocol
				       :_xim_xconnect))))
  ;; (print (list-properties (root-window clx-xim)))
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
  "get XIM_SERVERS from root-window, and trans them into atom-name"

  (get-property (root-window clx-xim)
		:xim_servers
		:transform #'(lambda (atom-id)
			       (atom-name (display clx-xim) atom-id))
		))

(defun -clx-xim-check-server-name- (clx-xim)
  "make sure server-name from env is string= to IM server get from Window"
  (string= (concatenate 'string *xim-server-category* (server-name clx-xim))
	   (symbol-name (nth
			 (index (check-server (connect-state clx-xim)))
			 (server-atoms clx-xim)))))

(defun -check-next-server- (clx-xim)
  ((lambda (check-server)
     (setf (subphase check-server) :xim-connect-check-server-prepare
	   (index check-server) (1+ (index check-server)))
     (when (requestor-window check-server)
       (destroy-window (requestor-window check-server))))
   (check-server (connect-state clx-xim))))

(defun -clx-xim-check-server-prepare- (clx-xim)
  (setf (window (check-server (connect-state clx-xim)))
	(selection-owner (display clx-xim)
			 (nth
			  (index (check-server (connect-state clx-xim)))
			  (server-atoms clx-xim))))
  (format t "AAA")
  (unless (-clx-xim-check-server-name- clx-xim)
    (return-from -clx-xim-check-server-prepare- NIL))

  ;; (print (create-window :parent (root-window clx-xim)
  ;; 		       :depth 0;;use the depth of parent
  ;; 		       :x 0 :y 0
  ;; 		       :width 1 :height 1
  ;; 		       :class :input-output
  ;; 		       :visual (screen-root-visual (default-screen clx-xim))))

  ;; (print (root-window clx-xim))
  ;; (print (screen-root-visual (default-screen clx-xim)))

  (setf (requestor-window (check-server (connect-state clx-xim)))
	(create-window :parent (root-window clx-xim)
		       :depth 0;;use the depth of parent
		       :x 0 :y 0
		       :width 1 :height 1
		       :class :input-output
		       :visual (screen-root-visual (default-screen clx-xim)))))


(defun -clx-xim-check-server-transport- (clx-xim)
  ;; (print (find-atom (display clx-xim) :transport))
  ;; (print (list-properties (requestor-window (check-server (connect-state clx-xim)))))
  (convert-selection (nth
		      (index (check-server (connect-state clx-xim)))
		      (server-atoms clx-xim))
		     :transport
		     (requestor-window (check-server (connect-state clx-xim)))
		     :property (find-atom (display clx-xim) :transport))
  ;; (print (list-properties (requestor-window (check-server (connect-state clx-xim)))))
  )

(defun -clx-xim-check-transport- (address)
  (when (and (> (length address) (length *xim-transport-category*))
	     (search *xim-transport-category* address)
	     (search "X/" address))
    "X/");;for now we just consdier "X/"

  ;;take values apart <<<<
  ;; (when (and (> (length address) (length *xim-transport-category*))
  ;; 	     (search *xim-transport-category* address))
  ;;   (let ((=-pos (position #\= address)))
  ;;     (when =-pos
  ;; 	(uiop:split-string (subseq address (1+ =-pos)) :separator ","))))
  ;;take values apart <<<<
  )
(defun -clx-xim-check-server-tranport-wait- (clx-xim)
  ;; (print (list-properties (requestor-window (check-server (connect-state clx-xim)))))
  (let ((display (display clx-xim)))
    (event-case (display)
      ((:selection-notify) (window selection target property time)
       ;; (print ":selection-notify")
       (unless (eq window
		   (requestor-window (check-server (connect-state clx-xim))))
	 (return-from -clx-xim-check-server-tranport-wait- :action-yield))

       (unless property
	 (return-from -clx-xim-check-server-tranport-wait- :action-failed))
       ;; (print (list-properties (requestor-window (check-server (connect-state clx-xim)))))
       (let ((address (get-property window
				    :transport
				    :delete-p T
				    :transform #'code-char
				    :result-type 'string
				    )))
	 ;; (format t "add ~A~%" address)
	 (unless address
	   (return-from -clx-xim-check-server-tranport-wait- :action-failed))
	 (unless (setf (trans-addr clx-xim) (-clx-xim-check-transport- address))
	   (return-from -clx-xim-check-server-tranport-wait- :action-failed))
	 ;; (print window)
	 (destroy-window window)
	 (setf (requestor-window (check-server (connect-state clx-xim))) NIL
	       (im-window clx-xim) (window (check-server (connect-state clx-xim))))
	 (push (cons :server-name
		     (nth
		      (index (check-server (connect-state clx-xim)))
		      (server-atoms clx-xim)))
	       (atoms clx-xim))
	 ;; (print (atoms clx-xim))
	 (return-from -clx-xim-check-server-tranport-wait- :action-accept))))))

(defun -clx-xim-connect-prepare- (clx-xim)
  (setf (im-client-window clx-xim)
	(create-window :parent (root-window clx-xim)
		       :depth 0;;use the depth of parent
		       :x 0 :y 0
		       :width 1 :height 1
		       :class :input-output
		       :visual (screen-root-visual (default-screen clx-xim))))

  (send-event (im-window clx-xim)
	      :client-message
	      0
	      :window (im-window clx-xim)
	      :type :_xim_xconnect
	      :format 32
	      :data (list (window-id (im-client-window clx-xim))
			  0 0 0 0)
	      :propagate-p NIL))

;; (defun -clx-write-xim-message-header (message major-opcode minor-opcode length swap)
;;   )
(defun padding-data-list (data times)
  (if (>= (length data) times)
      data
      (padding-data-list (append data (list 0)) times)))



(defun -clx-send-xim-message- (display protocol-atom window data length atom-name)
  (format t "length ~A ~A~%" length *clx-xim-header-size*)
  (unless data
    (return-from -clx-send-xim-message- NIL))
  (if (> (+ length *clx-xim-header-size*)
	 *clx-xim-cm-data-size*)
      (progn (intern-atom display atom-name)
	     (let ((pro (get-property window atom-name :type :string)))
	       (format t "~%-clx-send-xim-message- ~A~%" pro))

	     ;; (when (get-property window atom-name :type :string)
	     ;;   (format t "~%-clx-send-xim-message- ~A~%" )
	     ;;   (change-property window atom-name
	     ;; 			data :string
	     ;; 			8
	     ;; 			:mode append)
	     ;;   (send-event window
	     ;; 		   :client-message
	     ;; 		   0
	     ;; 		   :window (window-id window)
	     ;; 		   :type protocol-atom
	     ;; 		   :format 32
	     ;; 		   :data (list length
	     ;; 			       (find-atom display atom-name)
	     ;; 			       0 0)
	     ;; 		   :propagate-p NIL)
	     ;;   )
	     )
      (progn (format t "~%send-event ~A ~A~%" data window)
	     (send-event window
			 :client-message
			 0
			 :window window
			 :type protocol-atom
			 :format 8
			 :data (padding-data-list data *clx-xim-cm-data-size*)
			 :propagate-p NIL)
	     (format t "~%send-event~%"))))

(defun -clx-xim-send-message- (clx-xim data length)
  (format t "~%-clx-xim-send-message-~%")
  (-clx-send-xim-message- (display clx-xim)
			  :_xim_protocol
			  (accept-win clx-xim)
			  data
			  length
			  (concatenate 'string
				       "_client" (write-to-string (connect-id clx-xim))
				       "_" (write-to-string (xim-sequence clx-xim))))
  (setf (xim-sequence clx-xim) (1+ (xim-sequence clx-xim))))

(defun -clx-xim-send-frame- (clx-xim frame)
  (format t "~%-clx-xim-send-frame-~%")
  (let ((packet-size (size-packet frame)))
    (-clx-xim-send-message- clx-xim
			    (append (obj-to-data (make-instance 'clx-im-packet-header-fr
								:major-opcode (clx-proto-frame-opcode frame)
								:minor-opcode 0
								:header-bytes (/ packet-size 4)))
				    (obj-to-data frame))
			    packet-size)))

(defun -clx-xim-connect-wait- (clx-xim)
  (let ((display (display clx-xim)))
    (event-case (display)
      ((:client-message) (window type format data)
       (unless (eq type :_xim_xconnect)
	 (return-from -clx-xim-connect-wait- :action-yield))
       (format t "~%-clx-xim-connect-wait- :~% >>~%~A~% ~A~% ~A~% ~A~%" window type format data)
       (setf (major-code clx-xim) 0
	     (minor-code clx-xim) 0
	     (accept-win clx-xim) (xlib::lookup-window (display clx-xim) (elt data 0)));TODO: get window obj

       (unless (-clx-xim-send-frame- clx-xim (make-instance 'clx-im-connect-fr
							    :byte-order (byte-order clx-xim)
							    :pad 0
							    :client-major-protocol-version 0
							    :client-minor-protocol-version 0
							    :protocol-size 0
							    :protocol-items NIL))
	 (return-from -clx-xim-connect-wait- :action-failed))
       :action-accept))))

(defun -clx-read-xim-message- (display window format data)
  (let ((header (make-instance 'clx-im-packet-header-fr))
	(message)
	(data (coerce data 'list)))
    (case format
      (8
       (setf header (-clx-xim-read-frame- header data)
	     message data))
      (32
       (let ((reply (get-property window (atom-name display (cadr data)))))
	 (when reply
	   (setf header (-clx-xim-read-frame- header reply)
		 message reply)))))
    (list header message)))

(defun -clx-xim-send-open- (clx-xim)
  (-clx-xim-send-frame- clx-xim (make-instance 'clx-xim-open-fr))
  (setf (open-state clx-xim) :xim-open-wait-open-reply)
  (print "-clx-xim-send-open-"))

(defun -clx-xim-connect-wait-reply- (clx-xim)
  (let ((display (display clx-xim)))
    (event-case (display)
      ((:client-message) (window type format data)
       (unless (eq type :_xim_protocol)
	 (return-from -clx-xim-connect-wait-reply- :action-yield))
       (format t "~%-clx-xim-connect-wait-reply :~% >>~%~A~% ~A~% ~A~% ~A~%" window type format data)
       (destructuring-bind (header message)
	   (-clx-read-xim-message- (display clx-xim)
				   (accept-win clx-xim)
				   format
				   data)
	 (unless (= (major-opcode header) *clx-xim-connect-reply*)
	   (return-from -clx-xim-connect-wait-reply- :action-yield))
	 (let ((reply-frame (-clx-xim-read-frame- (make-instance 'clx-im-connect-reply-fr)
						  message)))
	   (when (-clx-xim-send-open- clx-xim)
	     (return-from -clx-xim-connect-wait-reply- :action-accept))))))))

(defun -clx-xim-preconnect-im- (clx-xim)
  (print "-clx-xim-preconnect-im-")
  (print (state-phase (connect-state clx-xim)))
  ;; (print (subphase (check-server (connect-state clx-xim))))
  (block block-preconnect
    (case (state-phase (connect-state clx-xim))
      (:xim-connect-check-server
       (when (eq (length (server-atoms clx-xim))
		 (index (check-server (connect-state clx-xim))))
	 (setf (state-phase (connect-state clx-xim)) :xim-connect-fail)
	 (return-from block-preconnect))
       (case (subphase (check-server (connect-state clx-xim)))
	 (:xim-connect-check-server-prepare
	  (if (-clx-xim-check-server-prepare- clx-xim)
	      (setf (subphase (check-server (connect-state clx-xim)))
		    :xim-connect-check-server-locale)
	      (progn (-check-next-server- clx-xim)
		     (return-from block-preconnect))))
	 (:xim-connect-check-server-locale ;TODO:
	  (setf (subphase (check-server (connect-state clx-xim)))
		:xim-connect-check-server-transport))
	 (:xim-connect-check-server-transport
	  (-clx-xim-check-server-transport- clx-xim)
	  ;; (print ":xim-connect-check-server-transport")
	  (setf (subphase (check-server (connect-state clx-xim)))
		:xim-connect-check-server-transport-wait))
	 (:xim-connect-check-server-transport-wait

	  (case (-clx-xim-check-server-tranport-wait- clx-xim)
	    (:action-accept
	     (setf (state-phase (connect-state clx-xim)) :xim-connect-connect
		   (connect-subphase (connect-state clx-xim)) :xim-connect-connect-prepare))
	    (:action-failed
	     (-check-next-server- clx-xim))
	    (:action-yield
	     (return-from -clx-xim-preconnect-im- NIL)))
	  (return-from block-preconnect))))
      (:xim-connect-connect

       (case (connect-subphase (connect-state clx-xim))
	 (:xim-connect-connect-prepare
	  (-clx-xim-connect-prepare- clx-xim)
	  (setf (connect-subphase (connect-state clx-xim)) :xim-connect-connect-wait))
	 (:xim-connect-connect-wait
	  (case (-clx-xim-connect-wait- clx-xim)
	    (:action-accept
	     (setf (connect-subphase (connect-state clx-xim)) :xim-connect-connect-wait-reply)
	     (return-from block-preconnect))
	    (:action-failed
	     (setf (state-phase (connect-state clx-xim)) :xim-connect-fail)
	     (return-from -clx-xim-preconnect-im-)
	     (:action-yield
	      (return-from -clx-xim-preconnect-im- NIL))))
	  )
	 (:xim-connect-connect-wait-reply
	  ;; (format t ":xim-connect-connect-wait-reply ~A~%" (-clx-xim-connect-wait-reply- clx-xim))
	  ;; (setf (state-phase (connect-state clx-xim)) :xim-connect-done)
	  (case (-clx-xim-connect-wait-reply- clx-xim)
	    (:action-accept
	     (setf (state-phase (connect-state clx-xim)) :xim-connect-done)
	     (return-from block-preconnect))
	    (:action-failed
	     (setf (state-phase (connect-state clx-xim)) :xim-connect-fail)
	     (return-from block-preconnect))
	    (:action-yield
	     (return-from -clx-xim-preconnect-im- NIL)))
	  )))
      (otherwise (return-from -clx-xim-preconnect-im-))))
  (-clx-xim-preconnect-im- clx-xim))

(defun clx-xim-property-changed (clx-xim)
  (event-case (display)
    ((:property-notify) (window atom state time)
     (when (and (eq (root-window clx-xim)
		    window)
		(eq :xim_servers
		    atom))
       (setf (recheck clx-xim) T)
       (when (eq :xim-connect-fail
		 (state-phase (connect-state clx-xim)))
	 (setf (yield-recheck clx-xim) T))))))

(defun -clx-xim-open- (clx-xim)
  (setf (open-state clx-xim) :xim-open-invalid
	(state-phase (connect-state clx-xim)) :xim-connect-fail)

  (unless (-clx-xim-init- clx-xim)
    (return-from -clx-xim-open- NIL))

  (when (auto-connect clx-xim)
    (-clx-change-event-mask- (root-window clx-xim) :property-change NIL))

  (unless (setf (server-atoms clx-xim) (-clx-xim-get-servers- clx-xim))
    (return-from -clx-xim-open- NIL))

  (setf (state-phase (connect-state clx-xim)) :xim-connect-check-server)
  ((lambda (check-server)
     (format t "~A~%" check-server)
     (setf (index check-server) 0)
     (format t "here")
     (setf (requestor-window check-server) 0)
     (setf (window check-server) 0)
     (setf (subphase check-server) :xim-connect-check-server-prepare))
   (check-server (connect-state clx-xim)))
  (-clx-xim-preconnect-im- clx-xim))


(defun clx-xim-open (clx-xim clx-xim-open-callback auto-connect user-data)
  (format t "here1~%")
  (format t "make ~A~%" (check-server (connect-state clx-xim)))
  (funcall (lambda (connect-state)
	     (setf (callback connect-state) clx-xim-open-callback
		   (user-data connect-state) user-data))
	   (connect-state clx-xim))
  (setf (auto-connect clx-xim) auto-connect)
  (format t "here2~%")
  (-clx-xim-open- clx-xim)
  (format t "here2"))

;; (defun clx-xim-process (clx-xim)
;;   (let (())
;;     (event-case ())))

(defun clx-xim-client-message (clx-xim format data)
  (destructuring-bind (header message)
      (-clx-read-xim-message- (display clx-xim) (im-client-window clx-xim) format data)
    (-clx-xim-handle-message- clx-xim header message (major-opcode header))
    ;; (format t "~%major-opcode ~A~%" (major-opcode header))
    ;; (format t "~%minor-opcode ~A~%" (minor-opcode header))
    ;; (format t "~%header-bytes ~A~%" (header-bytes header))
    ;; (format t "~%header: ~A message: ~A~%" header message)
    )
  )
