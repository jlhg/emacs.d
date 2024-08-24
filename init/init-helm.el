(add-to-list 'load-path "~/.emacs.d/package/emacs-async")
(add-to-list 'load-path "~/.emacs.d/package/helm")

(require 'helm)
(require 'helm-autoloads)

;; This program makes your CSS/SCSS/LESS coding faster
;; and easier than ever.
;; https://github.com/ShingoFukuyama/helm-css-scss
(require 'helm-css-scss)

(provide 'init-helm)
