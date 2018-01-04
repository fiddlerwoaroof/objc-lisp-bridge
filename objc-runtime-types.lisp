(in-package :objc-runtime)

(include "Foundation/NSGeometry.h")
(include "AppKit/NSWindow.h")
(include "AppKit/NSGraphics.h")

(cc-flags "-x objective-c -framework Foundation -framework AppKit -ObjC")

(cstruct ns-point "NSPoint"
         (ns-point-x "x" :type :double)
         (ns-point-y "y" :type :double))

(cstruct ns-size "NSSize"
         (ns-size-width "width" :type :double)
         (ns-size-height "height" :type :double))

(cstruct ns-rect "NSRect"
         (ns-rect-origin "origin" :type (:struct ns-point))
         (ns-rect-size "size" :type (:struct ns-size)))

(cenum (ns-window-style-mask)
       ((:ns-window-style-mask-borderless "NSWindowStyleMaskBorderless"))
       ((:ns-window-style-mask-titled "NSWindowStyleMaskTitled"))
       ((:ns-window-style-mask-closable "NSWindowStyleMaskClosable"))
       ((:ns-window-style-mask-miniaturizable "NSWindowStyleMaskMiniaturizable"))
       ((:ns-window-style-mask-resizable "NSWindowStyleMaskResizable")))

(cenum (ns-backing-store-type)
       ((:ns-backing-store-retained "NSBackingStoreRetained"))
       ((:ns-backing-store-Nonretained "NSBackingStoreNonretained"))
       ((:ns-backing-store-buffered "NSBackingStoreBuffered")))
