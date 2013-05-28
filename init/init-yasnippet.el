(add-to-list 'load-path "~/.emacs.d/package/yasnippet-0.8.0")

;; yas/trigger-key "C-c C-i"
(require 'yasnippet)
(yas/global-mode 1)
(yas/minor-mode-on)
(setq yas/prompt-functions
      '(yas/dropdown-prompt
        yas/x-prompt
        yas/completing-prompt
        yas/ido-prompt
        yas/no-prompt))

(provide 'init-yasnippet)
