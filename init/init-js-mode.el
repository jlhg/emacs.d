;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(eval-after-load "js2-mode"
  '(progn
     (setq js2-missing-semi-one-line-override t)
     (setq js2-strict-missing-semi-warning nil)
     (setq-default js2-basic-offset 2)))

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

;; tern-mode
(add-to-list 'load-path "~/.emacs.d/package/tern")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(defun my-tern-ac-complete ()
  "Complete code at point by tern."
  (interactive)
  (tern-ac-complete-request
   (lambda ()
     (let ((ac-sources (cons 'ac-source-tern-completion ac-sources)))
       (auto-complete)))))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (setq tern-ac-on-dot nil)
     (define-key tern-mode-keymap (kbd "C-.") 'my-tern-ac-complete)
     (tern-ac-setup)))

;; skewer-mode
(add-to-list 'load-path "~/.emacs.d/package/emacs-web-server")
(add-to-list 'load-path "~/.emacs.d/package/skewer-mode")
(require 'skewer-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;; nodejs-repl
(require 'nodejs-repl)

(provide 'init-js-mode)
