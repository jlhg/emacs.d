(setq c-default-style "linux")
(add-hook 'c-mode-hook
          '(lambda ()
             (setq ac-sources (delq 'ac-source-yasnippet ac-sources))
             (setq comment-style 'multi-line)))

(provide 'init-c-mode)
