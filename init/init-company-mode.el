(add-to-list 'load-path "~/.emacs.d/package/company-mode")

(add-hook 'after-init-hook 'global-company-mode)

;; Always turned on except in text-mode buffer.
(setq company-global-modes '(not text-mode json-mode yaml-mode magit-status-mode))

;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

;; The minimum prefix length for idle completion.
(setq company-minimum-prefix-length 3)

(provide 'init-company-mode)
