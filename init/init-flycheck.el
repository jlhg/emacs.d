(add-to-list 'load-path "~/.emacs.d/package/flycheck")
(add-to-list 'load-path "~/.emacs.d/package/s.el")
(add-to-list 'load-path "~/.emacs.d/package/dash.el")
(add-to-list 'load-path "~/.emacs.d/package/f.el")

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers
              '(emacs-lisp-checkdoc sh-bash sh-zsh))

;; https://github.com/flycheck/flycheck/issues/1559#issuecomment-478569550
(setq flycheck-emacs-lisp-load-path 'inherit)

(provide 'init-flycheck)
