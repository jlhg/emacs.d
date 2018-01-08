(require 'indent-guide)

;; If you want to enable indent-guide-mode in all buffers, call function indent-guide-global-mode.
(indent-guide-global-mode)

;; To show not only one guide line but all guide lines recursively, set “indent-guide-recursive” non-nil.
(setq indent-guide-recursive nil)

;; Config for Highlight-Indentation-for-Emacs
;; (add-to-list 'load-path "~/.emacs.d/package/Highlight-Indentation-for-Emacs")
;; (require 'highlight-indentation)

;; (define-globalized-minor-mode global-highlight-indentation-mode highlight-indentation-mode
;;   (lambda ()
;;     ;; (highlight-indentation-mode 1)
;;     (highlight-indentation-current-column-mode 1)
;;     ))

;; (global-highlight-indentation-mode 1)

(provide 'init-indent-guide)
