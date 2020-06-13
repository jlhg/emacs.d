(add-to-list 'load-path "~/.emacs.d/package/highlight-indent-guides")

(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?\|)
(setq highlight-indent-guides-responsive 'top)
(setq highlight-indent-guides-delay 0)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(provide 'init-highlight-indent-guides)
