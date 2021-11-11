(add-to-list 'load-path "~/.emacs.d/package/dumb-jump")
(require 'dumb-jump)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
