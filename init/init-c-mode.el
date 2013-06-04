(add-hook 'c-mode-hook
          '(lambda ()
             (setq c-default-style "linux"
                   c-basic-offset 4)
             (setq ac-sources (delq 'ac-source-yasnippet ac-sources))))

(provide 'init-c-mode)
