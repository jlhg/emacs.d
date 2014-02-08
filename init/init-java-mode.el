(add-hook 'java-mode-hook
          '(lambda ()
             (flymake-mode t)))

(provide 'init-java-mode)
