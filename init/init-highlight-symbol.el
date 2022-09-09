(require 'highlight-symbol)
(global-set-key [(f8)] 'highlight-symbol-at-point)
;; (global-set-key [f3] 'highlight-symbol-next)
;; (global-set-key [(shift f3)] 'highlight-symbol-prev)
;; (global-set-key [(meta f3)] 'highlight-symbol-prev)

;; Nord theme Aurora colors
(setq highlight-symbol-colors
      '("#bf616a" "#d08770" "#ebcb8b" "#a3be8c" "#b48ead"))

(provide 'init-highlight-symbol)
