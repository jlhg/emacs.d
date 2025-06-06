(add-to-list 'load-path "~/.emacs.d/package/company-mode")

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Always turned on except in text-mode buffer.
(setq company-global-modes '(not magit-status-mode))

;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

;; The minimum prefix length for idle completion.
(setq company-minimum-prefix-length 1)

;; Disable icons
(setq company-format-margin-function nil)

(provide 'init-company-mode)
