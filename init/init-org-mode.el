(add-to-list 'load-path "~/.emacs.d/package/org/list")

(require 'org-install)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(provide 'init-org-mode)
