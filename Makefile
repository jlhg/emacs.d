.PHONY: all
all:
		emacs --batch --eval '(byte-recompile-directory "~/.emacs.d")'
		mkdir backup
		chmod 700 backup
		cd package/magit && make
