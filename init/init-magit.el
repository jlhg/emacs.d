(add-to-list 'load-path "~/.emacs.d/package/compat")
(add-to-list 'load-path "~/.emacs.d/package/with-editor/lisp")
(add-to-list 'load-path "~/.emacs.d/package/dash.el")
(add-to-list 'load-path "~/.emacs.d/package/magit/lisp")
(add-to-list 'load-path "~/.emacs.d/package/magit-popup")
(add-to-list 'load-path "~/.emacs.d/package/ghub")
(add-to-list 'load-path "~/.emacs.d/package/transient/lisp")
(add-to-list 'load-path "~/.emacs.d/package/libegit2")
(add-to-list 'load-path "~/.emacs.d/package/graphql.el")
(add-to-list 'load-path "~/.emacs.d/package/treepy.el")

(add-to-list 'load-path "~/.emacs.d/package/yaml.el")
(add-to-list 'load-path "~/.emacs.d/package/emacsql")
(add-to-list 'load-path "~/.emacs.d/package/closql")
(add-to-list 'load-path "~/.emacs.d/package/forge/lisp")

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(setq magit-section-visibility-indicator nil)

;; https://www.reddit.com/r/emacs/comments/bdsfb7/comment/el0lowt/?utm_source=share&utm_medium=web2x&context=3
;; https://emacs.stackexchange.com/a/52040
;; https://irreal.org/blog/?p=8877
(setq magit-section-initial-visibility-alist
        '((unpushed . show)))

(setq transient-default-level 7)

(with-eval-after-load 'magit
  (require 'forge))

(provide 'init-magit)
