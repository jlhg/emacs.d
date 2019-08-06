(add-to-list 'load-path "~/.emacs.d/package/company-mode")
(add-to-list 'load-path "~/.emacs.d/package/unicode-escape.el")
(add-to-list 'load-path "~/.emacs.d/package/names")
(add-to-list 'load-path "~/.emacs.d/package/company-tabnine")

(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)

(provide 'init-company-tabnine)
