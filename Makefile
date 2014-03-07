.PHONY: all
all:
		emacs --batch --eval '(byte-recompile-directory ".")'
		mkdir -p backup
		chmod 700 backup
		cd package/magit && make
