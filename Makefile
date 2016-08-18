.PHONY: all
pkg_dir = $(shell pwd)/package
magit_dir = ${pkg_dir}/magit
dash_dir = ${pkg_dir}/dash.el
helm_dir = ${pkg_dir}/helm

all:
	emacs --batch --eval '(byte-recompile-directory ".")'
	mkdir -p backup erc/log
	chmod 700 backup
	cd ${magit_dir} && echo "LOAD_PATH = -L ${magit_dir}/lisp -L ${dash_dir} -L ${pkg_dir}" >config.mk && make
	cd ${helm_dir} && make
