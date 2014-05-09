;; indentation: 2 spaces
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2)
            (setq sh-indentation 2)))

(provide 'init-sh-mode)
