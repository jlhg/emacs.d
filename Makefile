.PHONY: all
pkg_dir = $(shell pwd)/package
magit_dir = ${pkg_dir}/magit
dash_dir = ${pkg_dir}/dash.el
helm_dir = ${pkg_dir}/helm
magit_popup_dir = ${pkg_dir}/magit-popup
ghub_dir = ${pkg_dir}/ghub
with_editor_dir = ${pkg_dir}/with-editor
graphql_dir = ${pkg_dir}/graphql.el
treepy_dir = ${pkg_dir}/treepy.el
async_dir = ${pkg_dir}/emacs-async

all:
	emacs --batch --eval '(byte-recompile-directory ".")'
	mkdir -p backup erc/log
	chmod 700 backup
	cd ${magit_dir} && echo "LOAD_PATH = -L ${magit_dir}/lisp -L ${dash_dir} -L ${magit_popup_dir} -L ${ghub_dir} -L ${with_editor_dir} -L ${graphql_dir} -L ${treepy_dir} -L ${pkg_dir}" >config.mk && make clean && make
	cd ${helm_dir} && make clean && EMACSLOADPATH="${async_dir}:" make
