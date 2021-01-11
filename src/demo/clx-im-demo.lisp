(defpackage #:demo
  (:use #:cl
	#:xlib
	#:clx-im))

(in-package #:demo)



(let ((display nil)
      (window nil)
      (event-mask (make-event-mask :key-press
				   :key-release
				   :focus-change)))
  (defun make-window ()
    (setf display (open-default-display))
    (let* ((screen (display-default-screen display))
	   (fg-color (screen-white-pixel screen))
	   (bg-color (screen-black-pixel screen))

	   (root (screen-root screen)))
      (setf window (create-window :parent root
				  :x 10 :y 20
				  :event-mask '(:visibility-change)
				  :background fg-color
				  :width 10 :height 100
				  ;; :border-width 2
				  :override-redirect :off))))
  (defun get-window ()
    window)
  (defun pre-display ()
    (setf (window-event-mask window) event-mask)
    (print (make-event-keys (window-event-mask window)))
    (print "i' ready")
    )
  (defun display-window ()
    (pre-display)
    (map-window window)
    (display-finish-output display)
    )

  (defun event-loop ()
    "doc"
    (event-case (display)
		((:key-press :key-release) (window code)
		 (print "got you")
		 (print window)
		 (print code))
		)
    (event-loop))

  (defun start-window ()
    (make-window)
    (display-window)
    (event-loop)
    ))

(defun start-demo ()
  (start-window))
(start-demo)
