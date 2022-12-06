(add-to-list 'load-path "~/.emacs.d/package/elisp-tree-sitter/core")
(add-to-list 'load-path "~/.emacs.d/package/elisp-tree-sitter/lisp")
(add-to-list 'load-path "~/.emacs.d/package/elisp-tree-sitter/langs")

(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-debug)
(require 'tree-sitter-query)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; ts-fold
;; https://github.com/emacs-tree-sitter/ts-fold
(add-to-list 'load-path "~/.emacs.d/package/fringe-helper.el")
(add-to-list 'load-path "~/.emacs.d/package/ts-fold")
(require 'ts-fold-indicators)
(require 'ts-fold)

(global-set-key (kbd "C-c f") 'ts-fold-toggle)

(provide 'init-tree-sitter)
