(add-to-list 'load-path "~/.emacs.d/package/company-mode")

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Always turned on except in text-mode buffer.
(setq company-global-modes '(not magit-status-mode))

;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-quick-access t)

;; The minimum prefix length for idle completion.
(setq company-minimum-prefix-length 1)

;; Disable icons
(setq company-format-margin-function nil)

;; When the candidate window is active, use M-n/M-p to navigate items.
(with-eval-after-load 'company
  (keymap-unset company-active-map "C-n" 'remove)
  (keymap-unset company-active-map "C-p" 'remove)
  (define-key company-active-map (kbd "M-n") #'company-select-next)
  (define-key company-active-map (kbd "M-p") #'company-select-previous))

(provide 'init-company-mode)
