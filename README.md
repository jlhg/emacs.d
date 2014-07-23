# Emacs configuration

## Features

* Python (Elpy)
* Java (Auto Java Complete)
* Javascript
* LaTeX (AUCTeX)
* Lisp (SLIME)
* Markdown
* Org mode
* R (ESS)
* Git (Magit)
* Auto complete (Auto Complete and YASnippet)
* On-the-fly syntax checks (FlyMake)

## Requirements

* emacs >= 24.3
* xclip
* ack
* r (ESS)
* texlive (AUCTeX)
* git (Magit)
* clisp (SLIME)
* Python libraries listed in `requirements/python-package.txt` (Python 2)
and `requirements/python3-package.txt` (Python 3) (Elpy)

## Optional requirements

* Tern (js2-mode)

## Setting up

Clone this repository to your `$HOME/.emacs.d` directory:

```bash
$ git clone --recursive https://github.com/jlhg/emacs.d ~/.emacs.d
```

After the cloning, create a symbolic link to `~/init.el`:

```bash
$ ln -s ~/.emacs.d/init.el ~/
```

Compilation:

```bash
$ cd ~/.emacs.d/
$ make
```

Install the Python libraries required for Elpy:

```bash
## In Python 2.7 environment
$ pip install -r ~/.emacs.d/requirements/python-package.txt

## In Python 3 environment
$ pip install -r ~/.emacs.d/requirements/python3-pacakge.txt

$ cp ~/.emacs.d/requirements/flake8-checker.sh /path/to/executable/path
```

Create tags for Auto Java Complete in java-mode (optional)

```bash
$ cd ~/.emacs.d/requirements
$ javac Tags.java
$ java -cp .:/path/to/your/jars_and_classesfiles/:/path/to/jre/lib/rt.jar Tags
## It will generate a file ~/.java_base.tag in your home directory
```

## Keymaps

### Global

* `M-;`: comment-dwim-line
* `C-_`: undo-tree-undo
* `M-_`: undo-tree-redo
* `M-up`: move-line-up
* `M-down`: move-line-down
* `C-j`: end-of-line-and-indent-new-line
* `TAB` or `C-i`: yas-expand
* `M-#`: query-replace-regexp

### SLIME

* `M-x slime`: Start SLIME REPL
* `M- M-x slime RET <lisp>`: Start SLIME REPL with specified lisp program

*SLIME REPL*

* `, q`: Quit SLIME

[more](http://common-lisp.net/project/slime/doc/html/REPL.html#REPL)

*lisp-mode*

* `C-c C-k`: slime-compile-and-load-file
* `C-c C-c`: slime-compile-defun
* `C-up`: slime-repl-forward-input
* `C-down`: slime-repl-backward-input


### Python

*python-mode*

* `C-c C-r`: elpy-refactor

[more](https://github.com/jorgenschaefer/elpy/wiki/Keybindings)
