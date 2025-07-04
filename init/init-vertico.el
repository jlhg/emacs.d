(add-to-list 'load-path "~/.emacs.d/package/compat")
(add-to-list 'load-path "~/.emacs.d/package/orderless")
(add-to-list 'load-path "~/.emacs.d/package/vertico")

(require 'vertico)
(vertico-mode t)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(provide 'init-vertico)
