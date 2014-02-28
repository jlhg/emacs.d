# Emacs configuration

## Features

* Python (Elpy)
* Java (Auto Java Complete)
* LaTeX (AUCTeX)
* Lisp (SLIME)
* Markdown
* Org mode
* Jinja2
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

## Setting up

Clone this repository to your `$HOME/.emacs.d` directory:

```bash
$ git clone --recursive https://github.com/jlhg/emacs.d ~/.emacs.d
```

After the cloning, create a symbolic link to `~/init.el`:

```bash
$ ln -s ~/.emacs.d/init.el ~/
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

Compile all lisp files:

```bash
$ emacs --batch --eval '(byte-recompile-directory "~/.emacs.d")'
```

Emacs will save backup files to `~/.emacs.d/backup`. For security, you need to
change the permission of this folder.

```bash
$ chmod 700 ~/.emacs.d/backup
```

## Global Keymaps

* `M-;`: comment or uncomment
* `C-_`: undo
* `M-_`: redo
* `M-up` and `M-down`: transpose lines
* `C-j`: insert a new line and jump to it
