(add-to-list 'load-path "~/.emacs.d/package/compat")
(add-to-list 'load-path "~/.emacs.d/package/orderless")
(add-to-list 'load-path "~/.emacs.d/package/vertico")
(add-to-list 'load-path "~/.emacs.d/package/vertico/extensions")

(require 'vertico)
(vertico-mode 1)

;; Enable vertico-grid for multi-column display
(require 'vertico-grid)
(vertico-grid-mode 1)

;; Ido-like directory navigation for Vertico
(require 'vertico-directory)
(with-eval-after-load 'vertico
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word))

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(provide 'init-vertico)
