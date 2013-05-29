# Emacs configuration

This configuration is mainly for Python. Also supports Latex, shell script,
markdown, org, jinja2, R and git.

Supports auto-complete, flymake, syntex checker, etc.

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
$ pip install -r ~/.emacs.d/requirements/python-package.txt --user
$ sudo dpkg --set-selections < ~/.emacs.d/requirements/package.txt; sudo apt-get dselect-upgrade
$ cp ~/.emacs.d/requirements/python-check.sh /path/to/executable/path
```

Download and install the newest version of Pymacs:

```bash
$ wget https://github.com/pinard/Pymacs/archive/master.zip
$ unzip master.zip
$ cd Pymacs-master/
$ make
$ python setup.py install --user
```

For using GitHub flavored markdown, go to https://github.com/Gagle/Node-GFM

Compile all lisp files:

```bash
$ emacs --batch --eval '(byte-recompile-directory "~/.emacs.d")'
```

## Global Keymaps

* `M-;`: comment or uncomment
* `C-_`: undo
* `M-_`: redo
* `M-up` and `M-down`: transpose lines
* `C-j`: insert a new line and jump to it
