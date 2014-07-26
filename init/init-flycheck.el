(add-to-list 'load-path "~/.emacs.d/package/flycheck")
(add-to-list 'load-path "~/.emacs.d/package/s.el")
(add-to-list 'load-path "~/.emacs.d/package/dash.el")
(add-to-list 'load-path "~/.emacs.d/package/f.el")
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'init-flycheck)
