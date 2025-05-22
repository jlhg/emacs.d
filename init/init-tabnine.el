(add-to-list 'load-path "~/.emacs.d/package/dash.el")
(add-to-list 'load-path "~/.emacs.d/package/s.el")
(add-to-list 'load-path "~/.emacs.d/package/transient/lisp")
(add-to-list 'load-path "~/.emacs.d/package/emacs-language-id")
(add-to-list 'load-path "~/.emacs.d/package/tabnine")
(require 'tabnine)

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

(with-eval-after-load 'tabnine
  ;; (kbd "TAB") is literal ctrl-I, (kbd "<tab>) is the actual tab key
  (define-key tabnine-completion-map (kbd "TAB") #'tabnine-accept-completion)
  (define-key tabnine-completion-map (kbd "<tab>") #'tabnine-accept-completion)

  (define-key tabnine-completion-map (kbd "M-f") #'tabnine-accept-completion-by-word)
  (define-key tabnine-completion-map (kbd "M-<return>") #'tabnine-accept-completion-by-line)

  (define-key tabnine-completion-map (kbd "C-g") #'tabnine-clear-overlay)
  (define-key tabnine-completion-map (kbd "M-[") #'tabnine-next-completion)
  (define-key tabnine-completion-map (kbd "M-]") #'tabnine-previous-completion))

;; Define a global minor mode for tabnine
(define-globalized-minor-mode global-tabnine-mode tabnine-mode
  (lambda () (tabnine-mode 1)))

;; Enable the global tabnine mode
(global-tabnine-mode 1)
;; (add-hook 'prog-mode-hook #'tabnine-mode)

(add-hook 'kill-emacs-hook #'tabnine-kill-process)

(provide 'init-tabnine)
