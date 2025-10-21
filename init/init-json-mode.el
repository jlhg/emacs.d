(require 'json-reformat)
(require 'json-snatcher)

(setq json-reformat:indent-width 2)
(setq json-reformat:pretty-string? t)

(require 'json-snatcher)
(require 'json-mode)

(add-hook 'json-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)
            (when (require 'auto-complete nil t)
              (auto-complete-mode t))))

(provide 'init-json-mode)
