;; use pyflakes + pep8 + pylint
(add-hook 'python-mode-hook
          '(lambda ()
             (setq python-check-command "python-check.sh")
             (define-key python-mode-map (kbd "C-c w") 'python-check)))

(add-hook 'python-mode-hook
   (lambda () (set (make-local-variable 'comment-inline-offset) 2)))

(provide 'init-python-mode)
