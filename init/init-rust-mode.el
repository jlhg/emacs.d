(add-to-list 'load-path "~/.emacs.d/package/rust-mode")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(provide 'init-rust-mode)
