(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
(add-hook  'markdown-mode-hook
           (lambda ()
             (setq indent-tabs-mode t)
             (setq auto-fill-mode -1)
             (setq visual-line-mode t)
             (setq visual-fill-column-mode t)
             (setq fill-column 120)
             (setq markdown-command "markdown")
             (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
               (auto-complete-mode t))))

(provide 'init-markdown-mode)
