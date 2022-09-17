all: build

quicklisp-projects:
	git clone --depth=1 https://github.com/quicklisp/quicklisp-projects.git

quicklisp-controller:
	git clone --depth=1 https://github.com/xach/githappy.git
	git clone --depth=1 https://github.com/xach/westbrook.git
	git clone --depth=1 https://github.com/quicklisp/project-info.git
	git clone --depth=1 https://github.com/quicklisp/quicklisp-controller.git

build: quicklisp-projects quicklisp-controller
	sbcl --load build.lisp
