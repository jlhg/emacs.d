(add-to-list 'load-path "~/.emacs.d/package/flycheck")
(add-to-list 'load-path "~/.emacs.d/package/s.el")
(add-to-list 'load-path "~/.emacs.d/package/dash.el")
(add-to-list 'load-path "~/.emacs.d/package/f.el")
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers
      '(python-flake8 python-pylint emacs-lisp-checkdoc sh-bash sh-zsh))


(provide 'init-flycheck)
