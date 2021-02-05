(ql:quickload :clx-xim/demo)
(ql:quickload :clx)
(demo::start-window)
(xlib:event-mask (demo::get-window))

(dotimes (step 2)
  (block b1
   (case step
     (1  (print setp)
      (print step))
     (2 (print "a")))))

(clx-xim/demo:sta)
(demo:start-demo)
