# quicklisp-systems

Search, browse and load Quicklisp systems from Emacs.

Author: Mariano Montone <marianomontone@gmail.com>

THIS IS WORK IN PROGRESS WORK.

![screenshot](screenshot.png "screenshot")

## Install

Load `swank` and add this repository path to `swank::*load-path*`, in your Lisp compiler init file (~/.sbclrc if using SBCL):

```lisp
(require :swank)
(push #p"/home/marian/src/lisp/quicklisp-systems/" swank::*load-path*)
```

In Emacs, add this repository path to `load-path` and add `quicklisp-systems` to `slime-contribs` in `~/.emacs` init file, like:

```
(push "/home/marian/src/lisp/quicklisp-systems" load-path)

(setq slime-contribs '(slime-fancy quicklisp-systems))

(slime-setup)
```

## Use

- `M-x quicklisp-systems-list`: browse the list of Quicklisp systems.
- `M-x quicklisp-systems-apropos`: search both by name and in system descriptions.
- `M-x quicklisp-systems-apropos-names`: search systems by name.
- `M-x quicklisp-systems-show-system`: show an Emacs buffer with information about the Quicklisp system.
- `M-x quicklisp-systems-update`: update the list of Quicklisp systems (this extension downloads a "systems file" with information of ASDF systems in Quicklisp to operate).

## License

MIT
