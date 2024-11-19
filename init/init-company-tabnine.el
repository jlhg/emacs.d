(add-to-list 'load-path "~/.emacs.d/package/company-mode")
(add-to-list 'load-path "~/.emacs.d/package/unicode-escape.el")
(add-to-list 'load-path "~/.emacs.d/package/names")
(add-to-list 'load-path "~/.emacs.d/package/company-tabnine")

(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)

;; Whether to overload company's minimum prefix length.
;; This allows completion to trigger on as much as possible.
(setq company-tabnine-always-trigger t)

(provide 'init-company-tabnine)
