(require 'go-mode-load)

(add-hook 'go-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)))

(require 'golint)

;; gocode
;; An autocompletion daemon for the Go programming language
;; https://github.com/nsf/gocode
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

(provide 'init-go-mode)
