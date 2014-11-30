(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))

;; indentation: 2 spaces
(eval-after-load 'css-mode
  '(progn
     (setq-default css-indent-offset 2)))

(provide 'init-css-mode)
