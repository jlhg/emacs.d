(setq exec-path (cons (expand-file-name "~/.rbenv/shims/") exec-path))
(add-to-list 'load-path "~/.emacs.d/package/scss-mode")
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(provide 'init-scss-mode)
