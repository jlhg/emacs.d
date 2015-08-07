(require 'indent-guide)

;; If you want to enable indent-guide-mode in all buffers, call function indent-guide-global-mode.
(indent-guide-global-mode)

;; To show not only one guide line but all guide lines recursively, set “indent-guide-recursive” non-nil.
(setq indent-guide-recursive nil)

(provide 'init-indent-guide)
