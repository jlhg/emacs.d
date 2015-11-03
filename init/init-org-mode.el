(add-to-list 'load-path "~/.emacs.d/package/org/list")

(require 'org-install)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-files '("~/.emacs.d/org-notes"))

;; Add the date to the CLOSED tag
(setq org-log-done 'time)

(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "CANCELED")))

(setq org-todo-keywords-for-agenda
      '("TODO" "DONE" "CANCELED"))

(setq org-todo-keyword-faces
      '(("CANCELED" . (:foreground "blue" :weight bold))))

(provide 'init-org-mode)
