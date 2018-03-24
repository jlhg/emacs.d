(require 'go-mode-load)

(add-hook 'go-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)))

(require 'golint)

(provide 'init-go-mode)
