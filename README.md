# Emacs configuration

This configuration is mainly for Python. Also supports Latex, shell script,
markdown, org, jinja2, R and git.

Supports auto-complete, flymake, syntex checker, etc.

## Requirements

emacs >= 24.3

## Setting up

Install pre-required packages:

```bash
$ pip install -r ~/.emacs.d/requirements/pip.txt
$ dpkg --set-selections < ~/.emacs.d/requirements/dpkg.txt; apt-get dselect-upgrade
$ cp ~/.emacs.d/requirements/python-check.sh /path/to/executable/path
```

Compile all el files:

```bash
$ emacs --batch --eval '(byte-recompile-directory "~/.emacs.d")'
```

## Global Keymaps

* `M-;`: comment or uncomment
* `C-_`: undo
* `M-_`: redo
* `M-up` and `M-down`: transpose lines
* `C-j`: insert a new line and jump to it
