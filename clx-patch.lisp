(xlib:declare-event :configure-request
  ((xlib::data (xlib::member8 :above :below :top-if :bottom-if :opposite))
   stack-mode)
  (xlib:card16 sequence)
  (xlib:window (parent event-window) window)
  ((or null xlib:window) above-sibling)
  (xlib:int16 x y)
  (xlib:card16 width height border-width value-mask))

