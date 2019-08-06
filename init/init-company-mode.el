(add-to-list 'load-path "~/.emacs.d/package/company-mode")

(add-hook 'after-init-hook 'global-company-mode)

;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

(provide 'init-company-mode)
