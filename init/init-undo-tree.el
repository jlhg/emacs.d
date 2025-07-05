(add-to-list 'load-path "~/.emacs.d/package/queue")

;; Undo with C-_ and redo with M-_. Watch the undo-tree with C-x u
(require 'queue)
(require 'undo-tree)
(global-undo-tree-mode t)

(provide 'init-undo-tree)
