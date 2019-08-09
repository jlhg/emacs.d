(add-to-list 'load-path "~/.emacs.d/package/go-mode.el")
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(add-hook 'go-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)))

(require 'golint)

(provide 'init-go-mode)
