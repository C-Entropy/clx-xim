(ql:quickload :clx-xim/demo)
(ql:quickload :clx)
(demo::start-window)
(xlib:event-mask (demo::get-window))
