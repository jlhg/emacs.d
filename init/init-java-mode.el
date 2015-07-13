(add-hook 'java-mode-hook
          '(lambda ()
             (flymake-mode t)
             (setq c-basic-offset 4)))


(provide 'init-java-mode)
