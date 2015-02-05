(add-to-list 'load-path "~/.emacs.d/package/elpy")
(add-to-list 'load-path "~/.emacs.d/package/company-mode")
(add-to-list 'load-path "~/.emacs.d/package/find-file-in-project")
(add-to-list 'load-path "~/.emacs.d/package/Highlight-Indentation-for-Emacs")
(add-to-list 'load-path "~/.emacs.d/package/idomenu")
(add-to-list 'load-path "~/.emacs.d/package/pyvenv")
(add-to-list 'load-path "~/.emacs.d/package/yasnippet")

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

;; Disable highlight-indentation-mode by default
(remove-hook 'elpy-modules 'elpy-module-highlight-indentation)

(provide 'init-elpy)
