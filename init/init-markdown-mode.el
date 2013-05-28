(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
(add-hook  'markdown-mode-hook
           (lambda ()
             (auto-fill-mode t)
             (setq fill-column 80)
             (setq markdown-command "gfm")
             (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
               (auto-complete-mode t))))

(provide 'init-markdown-mode)
