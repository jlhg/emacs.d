(require 'visual-fill-column)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

(add-hook  'markdown-mode-hook
           (lambda ()
             ;; (whitespace-mode)
             (auto-fill-mode -1)
             (visual-line-mode nil)
             ;; (visual-line-fill-column-mode t)
             ;; (setq fill-column 120)
             ;; (setq indent-tabs-mode t)
             (setq tab-width 2)
             ))

(provide 'init-markdown-mode)
