(defpackage #:demo
  (:use #:cl
	#:xlib
	#:clx-xim)
  (:export #:start-demo
	   #:clx-xim-client-message))

(in-package #:demo)

;; (fprintf(stderr, "Key %s Keycode %u, State %u\n",
;; 		   event->response_type == XCB_KEY_PRESS ? "press" : "release",
;; 		   event->detail, event->state)
;; )

(defun logger (fmt)
  (format t "~A~%" fmt))

(defun forward-event (clx-xim clx-xic key-press-event user-data)
  (format t "Key ~A Keycode ~A, State ~A~%" key-press-event))

(defun commit-string (clx-xim clx-xic flag str keysym nKeySym user-data)
  (format t "Key commit: ~A~%" input-content))

(defun open-callback (xcb-xim user-data)
  T)

(let ((clx-xim nil))
  (defun set-up-clx-xim (display screen
			 &key im-callback logger)
    (setf clx-xim (make-clx-xim display screen))
    (clx-xim-set-im-callback clx-xim im-callback NIL)
    (clx-xim-set-log-handler clx-xim logger)
    (clx-xim-open clx-xim #'open-callback T NIL))
  (defun print-clx-xim ()
    (format t "~A~%" (im-callback clx-xim)))
  (defun get-clx-xim ()
    clx-xim))


(let ((display nil)
      (screen nil)
      (window nil)
      (event-mask (make-event-mask :key-press
				   :key-release
				   :focus-change)))
  (defun get-window ()
    window)
  (defun get-screen ()
    screen)
  (defun get-display ()
    display)

  (defun make-window ()
    (setf display (open-default-display)
	  screen (display-default-screen display))
    (let ((fg-color (screen-white-pixel screen))
	  (bg-color (screen-black-pixel screen))
	  (root (screen-root screen)))
      (setf window (create-window :parent root
				  :x 10 :y 20
				  :event-mask '(:visibility-change)
				  :background fg-color
				  :width 10 :height 100
				  ;; :border-width 2
				  :override-redirect :off))))


  (defun pre-display ()
    (setf (window-event-mask window) event-mask)
    (print (make-event-keys (window-event-mask window)))
    (print "i' ready"))
  (defun display-window ()
    (pre-display)
    (map-window window)
    (display-finish-output display))

  (defun event-loop ()
    "doc"
    (event-case (display)
      ((:client-message) (window type format data)
       ;; (print ":client-message")
       (when (eq type :_xim_protocol)
	 ;; (format t "~%window: ~A type:  ~A format: ~A data: ~A~%" window type format data)
	 (clx-xim-client-message (get-clx-xim) format data)))

      ;; ((:selection-notify) (window selection target property time)
      ;;  (print ":selection-notify")
      ;;  (format t "~%window: ~A selection: ~A target: ~A property: ~A~%" window selection target property)
      ;;  )

      ;; ((:key-press :key-release) (window code)
      ;;  (print "catch you")
      ;;  (print window)
      ;;  (print code))
      )
    (event-loop)
    )

  (defun start-window ()
    (make-window)
    (display-window)
    (set-up-clx-xim display screen
		    :im-callback '((:forward-event . #'forward-event)
				   (:commit-string  . #'commit-string))
		    :logger #'logger)

    (event-loop)
    ))


(defun start-demo ()
  (format t "~%New demo~%")
  (start-window))

;; (find-atom (get-display) :xim_servers)
;; (change-property (get-window) :wm_name "hello1" :string 8 :transform #'char-code)
;; (get-property (get-window) :locales)
;; (find-atom (get-display) :locales)
;; (get-property (get-window) :wm_name ;; :type :string
;; 	      :transform #'code-char
;; 	      :result-type 'string
;; 	      )

;; (list-properties (get-window))
;; (get-property (get-window) :locales)
;; (intern-atom (get-display) :locales)

;; (change-property requestor property
;; 			       "Hello, World (from the CLX clipboard)!"
;; 			       target 8
;; 			       :transform #'char-code)
;; (start-demo)
