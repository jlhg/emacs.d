(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
(add-to-list 'auto-mode-alist
             '("\\.?\\(bashrc\\|bash_profile\\|profile\\)\\'"  . sh-mode))

(provide 'init-sh-mode)
