(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
(add-hook  'markdown-mode-hook
           (lambda ()
             (auto-fill-mode -1)
             (visual-line-mode t)
             (visual-fill-column-mode t)
             (setq fill-column 80)
             (setq markdown-command "markdown")
             (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
               (auto-complete-mode t))))

(provide 'init-markdown-mode)
