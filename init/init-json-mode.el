(require 'json-reformat)

(setq json-reformat:indent-width 2)
(setq json-reformat:pretty-string? t)

(require 'json-snatcher)
(require 'json-mode)

(add-hook 'json-mode-hook
          (lambda ()
            (setq tab-width 2)
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(provide 'init-json-mode)
