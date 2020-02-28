(add-to-list 'load-path "~/.emacs.d/package/go-mode.el")
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(require 'golint)

(provide 'init-go-mode)
