;; inf-ruby
(add-to-list 'load-path "~/.emacs.d/package/inf-ruby")
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;; robe-mode
;; https://github.com/dgutov/robe
;; Required gems:
;; pry
;; pry-doc >= 0.6.0 (on MRI)
;; method_source >= 0.8.2 (for compatibility with the latest Rubinius)
(add-to-list 'load-path "~/.emacs.d/package/robe")

(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)

;; yard-mode
;; https://github.com/pd/yard-mode.el
(add-to-list 'load-path "~/.emacs.d/package/yard-mode.el")
(require 'yard-mode)
(add-hook 'ruby-mode-hook 'yard-mode)
;; (add-hook 'ruby-mode-hook 'eldoc-mode)

(provide 'init-ruby-mode)
