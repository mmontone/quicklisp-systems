all: build

quicklisp-projects:
	git clone --depth=1 https://github.com/quicklisp/quicklisp-projects.git

quicklisp-controller:
	git clone --depth=1 https://github.com/xach/githappy.git
	git clone --depth=1 https://github.com/xach/westbrook.git
	git clone --depth=1 https://github.com/quicklisp/project-info.git
	git clone --depth=1 https://github.com/quicklisp/quicklisp-controller.git

systems-file:
	sbcl --load 'make-systems-file.lisp' --eval '(quicklisp-systems-file::make-systems-file)'

build: quicklisp-projects quicklisp-controller systems-file
	sbcl --load build.lisp


