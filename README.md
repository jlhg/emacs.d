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

* [Tern](http://ternjs.net/) (js2-mode)
* [Language checkers required for Flycheck](http://flycheck.readthedocs.org/en/latest/guide/languages.html)

## Setting up

1. Clone this repository to your `$HOME/.emacs.d` directory:

```bash
$ git clone --recursive https://github.com/jlhg/emacs.d ~/.emacs.d
```

2. After the cloning, create a symbolic link to `~/init.el`:

```bash
$ ln -s ~/.emacs.d/init.el ~/
```

3. Compilation:

```bash
$ cd ~/.emacs.d/
$ make
```

4. Install the Python libraries required for python-mode:

```bash
# In Python 2.7 environment
$ pip install -r ~/.emacs.d/requirements/python-package.txt

# In Python 3 environment
$ pip install -r ~/.emacs.d/requirements/python3-pacakge.txt

$ cp ~/.emacs.d/requirements/flake8-checker.sh /path/to/executable/path
```

5. Create Auto Java Complete tags for java-mode:

```bash
$ cd ~/.emacs.d/requirements
$ javac Tags.java
$ java -cp .:/path/to/your/jars_and_classesfiles/:/path/to/jre/lib/rt.jar Tags
# This will generate a file .java_base.tag in your home directory
```

6. Install the npm modules for js2-mode:

```bash
$ npm install tern jshint
$ echo 'export PATH=/path/to/node_modules/.bin:$PATH' >>~/.bashrc
$ . ~/.bashrc
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

### Javascript

*js2-mode* *tern-mode*

* `M-.`: Jump to the definition of the thing under the cursor.
* `M-,`: Brings you back to last place you were when you pressed M-..
* `C-c C-r`: Rename the variable under the cursor.
* `C-c C-c`: Find the type of the thing under the cursor.
* `C-c C-d`: Find docs of the thing under the cursor. Press again to open the associated URL (if any).
