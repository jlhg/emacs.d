(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
(add-hook  'markdown-mode-hook
           (lambda ()
             (auto-fill-mode t)
             (setq fill-column 80)
             (setq indent-tabs-mode t)))

(provide 'init-markdown-mode)
