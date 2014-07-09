(require 'go-mode-load)

(add-hook 'go-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)))

(provide 'init-go-mode)
