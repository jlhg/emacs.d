(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "C-c w") 'python-check)
             (set (make-local-variable 'comment-inline-offset) 2)
             (setq python-shell-virtualenv-path (expand-file-name "~/pyenv"))))

(provide 'init-python-mode)
