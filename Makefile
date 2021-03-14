CL=sbcl

all: mkapp

demo-app:
	$(CL) --load ~/quicklisp/setup.lisp \
		    --eval '(ql:quickload :data-lens)' \
		   --load save.lisp

demo-app.iconset: demo-app.svg
	rm -rf demo-app.iconset
	mkdir -p demo-app.iconset
	zsh convert.sh demo-app.svg   16   demo-app.iconset/icon_16x16.png
	zsh convert.sh demo-app.svg   32   demo-app.iconset/icon_16x16@2x.png
	zsh convert.sh demo-app.svg   32   demo-app.iconset/icon_32x32.png
	zsh convert.sh demo-app.svg   64   demo-app.iconset/icon_32x32@2x.png
	zsh convert.sh demo-app.svg   64   demo-app.iconset/icon_64x64.png
	zsh convert.sh demo-app.svg  128   demo-app.iconset/icon_64x64@2x.png
	zsh convert.sh demo-app.svg  128  demo-app.iconset/icon_128x128.png
	zsh convert.sh demo-app.svg  256  demo-app.iconset/icon_128x128@2x.png
	zsh convert.sh demo-app.svg  256  demo-app.iconset/icon_256x256.png
	zsh convert.sh demo-app.svg  512  demo-app.iconset/icon_256x256@2x.png
	zsh convert.sh demo-app.svg  512  demo-app.iconset/icon_512x512.png
	zsh convert.sh demo-app.svg 1024  demo-app.iconset/icon_512x512@2x.png
	zsh convert.sh demo-app.svg 1024 demo-app.iconset/icon_1024x1024.png
	zsh convert.sh demo-app.svg 2048 demo-app.iconset/icon_1024x1024@2x.png

mkapp: demo-app demo-app.iconset
	rm -rf demo.app
	cp -R demo.app.template demo.app
	mkdir -p demo.app/Contents/{Resources,MacOS,Frameworks}
	iconutil -c icns demo-app.iconset -o demo.app/Contents/Resources/demo-app.icns
	ibtool --compile demo.app/Contents/Resources/MainMenu.nib MainMenu.xib
	cp demo-app demo.app/Contents/MacOS
