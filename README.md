# Emacs configuration

This configuration is mainly for Python. Also supports Latex, shell script,
markdown, org, jinja2, R and git.

Supports auto-complete, flymake, syntax checker, etc.

## Requirements

emacs >= 24.3

## Setting up

After download, move emacs.d to your home directory, and create link to `init.el`:

```bash
$ mv emacs.d ~/.emacs.d
$ ln -s ~/.emacs.d/init.el ~/
```

Install pre-required packages:

```bash
$ pip install -r ~/.emacs.d/requirements/python-package.txt
$ sudo dpkg --set-selections < ~/.emacs.d/requirements/package.txt; sudo apt-get dselect-upgrade
```

Download and install the newest version of Pymacs:

```bash
$ wget https://github.com/pinard/Pymacs/archive/master.zip
$ unzip master.zip
$ cd Pymacs-master/
$ make
$ pip install ./
```

For using GitHub flavored markdown, go to https://github.com/Gagle/Node-GFM

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
