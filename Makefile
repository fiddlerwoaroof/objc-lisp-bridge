CCL=ccl

dylib: nsrect-expose.m
	clang -shared \
			-framework Cocoa \
			nsrect-expose.m \
			-o libnsrect-expose.dylib

demo-app: dylib
	$(CCL) --load ~/quicklisp/setup.lisp \
           --load save.lisp

demo-app.iconset: demo-app.svg
	rm -rf demo-app.iconset
	mkdir -p demo-app.iconset
	rsvg-convert -h 16  demo-app.svg >	demo-app.iconset/icon_16x16.png
	rsvg-convert -h 32  demo-app.svg >	demo-app.iconset/icon_16x16@2x.png
	rsvg-convert -h 32  demo-app.svg >	demo-app.iconset/icon_32x32.png
	rsvg-convert -h 64  demo-app.svg >	demo-app.iconset/icon_32x32@2x.png
	rsvg-convert -h 64  demo-app.svg >	demo-app.iconset/icon_64x64.png
	rsvg-convert -h 128	demo-app.svg > demo-app.iconset/icon_64x64@2x.png
	rsvg-convert -h 128	demo-app.svg > demo-app.iconset/icon_128x128.png
	rsvg-convert -h 256	demo-app.svg > demo-app.iconset/icon_128x128@2x.png
	rsvg-convert -h 256	demo-app.svg > demo-app.iconset/icon_256x256.png
	rsvg-convert -h 512	demo-app.svg > demo-app.iconset/icon_256x256@2x.png
	rsvg-convert -h 512	demo-app.svg > demo-app.iconset/icon_512x512.png

mkapp: dylib demo-app demo-app.iconset
	rm -rf demo.app
	cp -R demo.app.template demo.app
	mkdir -p demo.app/Contents/{Resources,MacOS}
	iconutil -c icns demo-app.iconset -o demo.app/Contents/Resources/demo-app.icns
	ibtool --compile demo.app/Contents/Resources/MainMenu.nib MainMenu.xib
	cp demo-app libnsrect-expose.dylib demo.app/Contents/MacOS
