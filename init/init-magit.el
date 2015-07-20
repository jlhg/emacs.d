(add-to-list 'load-path "~/.emacs.d/package/dash.el")
(add-to-list 'load-path "~/.emacs.d/package/magit/lisp")

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-restore-window-configuration nil)

(provide 'init-magit)
