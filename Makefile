CCL=ccl

dylib: nsrect-expose.m
	clang -shared \
	      -framework Cocoa \
	      nsrect-expose.m \
	      -o libnsrect-expose.dylib
run: dylib
	$(CCL) --eval '(load (compile-file "objc-runtime.asd"))' \
	       --eval '(ql:quickload :objc-runtime)' \
	       --eval '(load (compile-file "demo-app.lisp"))' \
	       --eval '(demo-app::main)'
