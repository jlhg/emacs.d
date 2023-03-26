(add-hook 'python-mode-hook
    (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent-offset 4)))

(provide 'init-python-mode)
