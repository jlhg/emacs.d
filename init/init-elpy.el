(add-to-list 'load-path "~/.emacs.d/package/elpy-0.8")
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

(defvar ropemacs-enable-autoimport t
    "Specifies whether autoimport should be enabled.

Value set by elpy.")

(defvar ropemacs-guess-project t
    "Try to guess the project when needed.

If non-nil, ropemacs tries to guess and open the project that contains
a file on which the rope command is performed when no project is
already opened.

Value set by elpy.")

(defvar ropemacs-confirm-saving nil
    "Shows whether to confirm saving modified buffers before refactorings.

If non-nil, you have to confirm saving all modified
python files before refactorings; otherwise they are
saved automatically.

Value set by elpy.")

(defvar ropemacs-enable-shortcuts nil
    "Shows whether to bind ropemacs shortcuts keys.

Value set by elpy, as we set our own key bindings.")

(defvar ropemacs-local-prefix nil
    "The prefix for ropemacs refactorings.

Use nil to prevent binding keys.

Value set by elpy, as we set our own key bindings.")

(defvar ropemacs-global-prefix nil
    "The prefix for ropemacs project commands.

Use nil to prevent binding keys.

Value set by elpy, as we set our own key bindings.")


(provide 'init-elpy)
