(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinja?\\'" . web-mode))
(add-to-list 'web-mode-engine-file-regexps '("django" . "\\.html"))
(add-to-list 'web-mode-engine-file-regexps '("django" . "\\.jinja"))

(define-key web-mode-map (kbd "C-c /") 'web-mode-element-close)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-auto-pairing nil)
(set-face-attribute 'web-mode-current-element-highlight-face nil :background "dark slate gray")
(set-face-attribute 'web-mode-error-face nil :underline "red" :background nil)

(add-hook 'web-mode-hook
          '(lambda ()
             (auto-complete-mode 1)))

(provide 'init-web-mode)
