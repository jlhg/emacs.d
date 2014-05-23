(require 'php-mode)
(add-hook 'php-mode-hook
          (lambda ()
            (subword-mode 1)))

(provide 'init-php-mode)
