(add-to-list 'load-path "~/.emacs.d/package/php-mode")
(add-to-list 'load-path "~/.emacs.d/package/php-mode/skeleton")

(require 'php-mode)
(add-hook 'php-mode-hook
          (lambda ()
            (subword-mode 1)))

(eval-after-load 'php-mode
  '(require 'php-ext))

(provide 'init-php-mode)
