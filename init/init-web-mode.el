(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinja?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[tj]s[x]?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mako$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))

(add-to-list 'web-mode-engine-file-regexps '("django" . "\\.html"))
(add-to-list 'web-mode-engine-file-regexps '("django" . "\\.jinja"))
(setq web-mode-engines-alist
      '(("django"  . "\\.jinja\\'")
        ("django"  . "\\.djhtml\\'")
        ("django"  . "\\.html\\'")
        ("erb"     . "\\.erb\\'")
        ("erb"     . "\\.rhtml\\'")
        ("erb"     . "\\.ejs\\'")
        ("php"     . "\\.phtml\\'")
        ("php"     . "\\.php\\'")
        ("php"     . "\\.psp\\'")
        ("php"     . "\\.ctp\\'")
        ("jsp"     . "\\.jsp\\'")
        ("jsp"     . "\\.gsp\\'")
        ("asp"     . "\\.asp\\'")
        ("aspx"    . "\\.aspx\\'")
        ("aspx"    . "\\.ascx\\'")
        ("closure" . "\\.soy\\'")
        ("lsp"     . "\\.lsp\\'")
        ("mako"    . "\\.mako\\'")
        ("blade"   . "\\.blade\\.")
        ("svelte"  . "\\.svelte\\'"))
)

(define-key web-mode-map (kbd "C-c /") 'web-mode-element-close)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-auto-pairing nil)
(set-face-attribute 'web-mode-current-element-highlight-face nil :background "dark slate gray")
(set-face-attribute 'web-mode-error-face nil :underline "red" :background nil)

(add-hook 'web-mode-hook
          '(lambda ()
             (auto-complete-mode 1)
             (visual-line-mode 0)))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-script-padding 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-auto-quoting nil)
(setq web-mode-enable-current-element-highlight t)
(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-quotes" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
(add-to-list 'web-mode-indentation-params '("case-extra-offset" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode)
              ;; set comment start
              (set (make-local-variable 'comment-start) "// "))))

(provide 'init-web-mode)
