;; inf-ruby
;; (add-to-list 'load-path "~/.emacs.d/package/inf-ruby")
;; (autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
;; (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;; robe-mode
;; https://github.com/dgutov/robe
;; Required gems:
;; pry
;; pry-doc >= 0.6.0 (on MRI)
;; method_source >= 0.8.2 (for compatibility with the latest Rubinius)
;; (add-to-list 'load-path "~/.emacs.d/package/robe")

;; (require 'robe)
;; (add-hook 'ruby-mode-hook 'robe-mode)

;; yard-mode
;; https://github.com/pd/yard-mode.el
(add-to-list 'load-path "~/.emacs.d/package/yard-mode.el")
(require 'yard-mode)
(add-hook 'ruby-mode-hook 'yard-mode)
;; (add-hook 'ruby-mode-hook 'eldoc-mode)

;; Prevent to add coding information in the first line
(setq ruby-insert-encoding-magic-comment nil)

;; Ruby end
;; Ruby end is a minor mode for Emacs that can be used with
;; ruby-mode to automatically close blocks by inserting end when
;; typing a "block keyword", followed by a space.
;; https://github.com/rejeep/ruby-end.el
(add-to-list 'load-path "~/.emacs.d/package/ruby-end.el")
(require 'ruby-end)

(setq ruby-end-expand-only-for-last-commands nil)

;; RuboCop.el
;; https://github.com/rubocop/rubocop-emacs
(add-to-list 'load-path "~/.emacs.d/package/rubocop-emacs")
(require 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)

;; Enhanced Ruby Mode
;; https://github.com/zenspider/Enhanced-Ruby-Mode
;; (add-to-list 'load-path "~/.emacs.d/package/enhanced-ruby-mode") ; must be added after any path containing old ruby-mode
;; (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
;; (add-to-list 'auto-mode-alist
;;              '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
;; (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
;; ;; (add-hook 'enh-ruby-mode-hook 'robe-mode)
;; (add-hook 'enh-ruby-mode-hook 'yard-mode)
;; (add-hook 'enh-ruby-mode-hook 'rubocop-mode)
;; ;; (setq enh-ruby-add-encoding-comment-on-save nil)
;; (setq enh-ruby-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "{"             #'enh-ruby-electric-brace)
;;     (define-key map "}"             #'enh-ruby-electric-brace)
;;     (define-key map (kbd "M-C-a")   #'enh-ruby-beginning-of-defun)
;;     (define-key map (kbd "M-C-e")   #'enh-ruby-end-of-defun)
;;     (define-key map (kbd "M-C-b")   #'enh-ruby-backward-sexp)
;;     (define-key map (kbd "M-C-f")   #'enh-ruby-forward-sexp)
;;     (define-key map (kbd "M-C-p")   #'enh-ruby-beginning-of-block)
;;     (define-key map (kbd "M-C-n")   #'enh-ruby-end-of-block)
;;     (define-key map (kbd "M-C-h")   #'enh-ruby-mark-defun)
;;     (define-key map (kbd "M-C-q")   #'enh-ruby-indent-exp)
;;     (define-key map (kbd "C-c C-f") #'enh-ruby-find-file)
;;     (define-key map (kbd "C-c C-e") #'enh-ruby-find-error)
;;     (define-key map (kbd "C-c /")   #'enh-ruby-insert-end)
;;     (define-key map (kbd "C-c {")   #'enh-ruby-toggle-block)
;;     (define-key map (kbd "M-C-u")   #'enh-ruby-up-sexp)
;;     ;; (define-key map (kbd "C-j")     #'reindent-then-newline-and-indent)
;;     map))

(provide 'init-ruby-mode)
