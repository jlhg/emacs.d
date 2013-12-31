(setq c-default-style "linux")
(add-hook 'c-mode-hook
          '(lambda ()
             (setq ac-sources (delq 'ac-source-yasnippet ac-sources))
             (flymake-mode t)))

(provide 'init-c-mode)
