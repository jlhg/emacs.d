;;; Emacs configuration
;;; requirement: emacs >= 24.3

;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
(add-to-list 'load-path "~/.emacs.d/init")
(add-to-list 'load-path "~/.emacs.d/package")

(require 'init-global)
(require 'init-sh-mode)
(require 'init-python-mode)
(require 'init-css-mode)
(require 'init-java-mode)
(require 'init-c-mode)
(require 'init-cheetah-mode)

;; auto complete
(require 'init-auto-complete)

;; yasnippet
(require 'init-yasnippet)

;; Magit - an emacs mode for interacting with the Git version control system
;; http://magit.github.io/magit/index.html
(require 'init-magit)

;; insert-time
;; https://github.com/rmm5t/insert-time.el
(require 'init-insert-time)

;; jinja2-mode
;; https://github.com/paradoxxxzero/jinja2-mode
(require 'init-jinja2-mode)

;; markdown mode
;; http://jblevins.org/projects/markdown-mode/
;; requirements: gfm preview: https://github.com/Gagle/Node-GFM
(require 'init-markdown-mode)

;; ESS - Emacs Speaks Statistics
;; requirements: R
(require 'init-ess)

;; diff-mode-: extension to 'diff-mode.el'
;; change highlight colors
(require 'init-diff-mode-)

;; multiple cursors
;; https://github.com/emacsmirror/multiple-cursors
(require 'init-multiple-cursors)

;; ace jump mode
;; https://github.com/winterTTr/ace-jump-mode
(require 'init-ace-jump-mode)

;; flymake
(require 'init-flymake)

;; Elpy - the Emacs Lisp Python Environment
;; https://github.com/jorgenschaefer/elpy
(require 'init-elpy)

;; xclip - an interface to the xclip tool
;; requirements: xclip
(require 'init-xclip)

;; org mode
(require 'init-org-mode)

;; hightlight symbol
(require 'init-highlight-symbol)

;; vlf-mode - view large files
(require 'init-vlf-mode)

;; ido mode
;; version: 1.31.2.70
;; http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/emacs/lisp/ido.el?revision=1.31.2.70
(require 'init-ido-mode)

;; visible mark
;; http://retroj.net/visible-mark
(require 'init-visible-mark)

;; predictive mode
(require 'init-predictive-mode)

;; La Carte
(require 'init-larcarte)

;; anything
(require 'init-anything)

;; undo tree
(require 'init-undo-tree)

;; AUCTex (LaTeX)
;; requirements: texlive-full
(require 'init-auctex)

;; Moz
(require 'init-moz)

;; zotero
;; require MozRepl for firefox: https://github.com/bard/mozrepl/wiki
;; after installing MozRepl: FireFox -> Tools -> MozRepl -> Start
(require 'init-zotero-minor-mode)

;; erc
(require 'init-erc)
