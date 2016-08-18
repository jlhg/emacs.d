.PHONY: all
pkg_dir = $(shell pwd)/package
magit_dir = ${pkg_dir}/magit
dash_dir = ${pkg_dir}/dash.el

all:
	emacs --batch --eval '(byte-recompile-directory ".")'
	mkdir -p backup erc/log
	chmod 700 backup
	cd ${magit_dir} && echo "LOAD_PATH = -L ${magit_dir}/lisp -L ${dash_dir}" >config.mk && make
