# Emacs configuration

This configuration is mainly for Python development. Also supports LaTeX, shell script,
markdown, org, jinja2, R and git.

Supports auto-complete, flymake, syntax checker, etc.

## Requirements

emacs >= 24.3

## Setting up

Clone this repository to your `$HOME/.emacs.d` directory:

```bash
$ git clone --recursive https://github.com/jlhg/emacs.d ~/.emacs.d
```

After the cloning, create a symbolic link to `~/init.el`:

```bash
$ ln -s ~/.emacs.d/init.el ~/
```

Install pre-required packages:

```bash
## In Python 2.7 environment
$ pip install -r ~/.emacs.d/requirements/python-package.txt

## In Python 3 environment
$ pip install -r ~/.emacs.d/requirements/python3-pacakge.txt

$ cp ~/.emacs.d/requirements/flake8-checker.sh /path/to/executable/path
$ sudo dpkg --set-selections < ~/.emacs.d/requirements/package.txt; sudo apt-get dselect-upgrade
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
$ chmod go-rwx ~/.emacs.d/backup
```



## Global Keymaps

* `M-;`: comment or uncomment
* `C-_`: undo
* `M-_`: redo
* `M-up` and `M-down`: transpose lines
* `C-j`: insert a new line and jump to it
