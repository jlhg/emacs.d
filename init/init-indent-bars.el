(add-to-list 'load-path "~/.emacs.d/package/indent-bars")

(require 'indent-bars)

(setq indent-bars-color '(highlight :face-bg t :blend 0))
(setq indent-bars-display-on-blank-lines nil)

(add-hook 'prog-mode-hook 'indent-bars-mode)
(add-hook 'yaml-mode-hook 'indent-bars-mode)

(provide 'init-indent-bars)