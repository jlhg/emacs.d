(add-to-list 'load-path "~/.emacs.d/package/elpy-1.3.0")
(add-to-list 'load-path "~/.emacs.d/package/auto-complete-1.4")
(add-to-list 'load-path "~/.emacs.d/package/fuzzy-0.1")
(add-to-list 'load-path "~/.emacs.d/package/yasnippet-0.8.0")
(add-to-list 'load-path "~/.emacs.d/package/virtualenv-1.2")
(add-to-list 'load-path "~/.emacs.d/package/highlight-indentation-0.5.0")
(add-to-list 'load-path "~/.emacs.d/package/find-file-in-project-3.2")
(add-to-list 'load-path "~/.emacs.d/package/idomenu-0.1")
(add-to-list 'load-path "~/.emacs.d/package/nose-0.1.1")

(require 'elpy)
(elpy-enable)
;; (elpy-use-ipython)
;; (elpy-clean-modeline)

(setq python-check-command "flake8-checker.sh")
(setq elpy-rpc-backend "jedi")

(defvar flymake-no-changes-timeout 60
    "Time to wait after last change before starting compilation.

The original value of 0.5 is too short for Python code, as that
will result in the current line to be highlighted most of the
time, and that's annoying. This value might be on the long side,
but at least it does not, in general, interfere with normal
interaction.

Value set by elpy.")

(defvar flymake-start-syntax-check-on-newline nil
    "Start syntax check if newline char was added/removed from the buffer.

This should be nil for Python, as most lines with a colon at the
end will mean the next line is always highlighted as error, which
is not helpful and mostly annoying.

Value set by elpy.")

(provide 'init-elpy)
