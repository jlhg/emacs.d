;;; Emacs configuration
;;; requirement: emacs >= 24.3

;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/init")
(add-to-list 'load-path "~/.emacs.d/package")

;; For Emacs 28 native-comp
(require 'init-native-comp)

;; Theme
(require 'init-theme)

(require 'init-global)
(require 'init-ibuffer)
(require 'init-auto-minor-mode)

;; Visual Fill Column
;; https://github.com/joostkremers/visual-fill-column
(require 'init-visual-fill-column)

;; auto complete
(require 'init-auto-complete)

;; yasnippet
(require 'init-yasnippet)

;; company-mode
;; Modular in-buffer completion framework for Emacs
;; http://company-mode.github.io/
(require 'init-company-mode)

;; Magit - an emacs mode for interacting with the Git version control system
;; http://magit.github.io/magit/index.html
(require 'init-magit)

;; insert-time
;; https://github.com/rmm5t/insert-time.el
(require 'init-insert-time)

;; jinja2-mode
;; https://github.com/paradoxxxzero/jinja2-mode
;; (require 'init-jinja2-mode)

;; markdown mode
;; http://jblevins.org/projects/markdown-mode/
;; requirements: gfm preview: https://github.com/Gagle/Node-GFM
(require 'init-markdown-mode)

;; ESS - Emacs Speaks Statistics
;; requirements: R
;; (require 'init-ess)

;; diff-mode-: extension to 'diff-mode.el'
;; change highlight colors
;; (require 'init-diff-mode-)

;; multiple cursors
;; https://github.com/emacsmirror/multiple-cursors
(require 'init-multiple-cursors)

;; ace jump mode
;; https://github.com/winterTTr/ace-jump-mode
;; (require 'init-ace-jump-mode)

;; flycheck
(require 'init-flycheck)

;; Elpy - the Emacs Lisp Python Environment
;; https://github.com/jorgenschaefer/elpy
;; (require 'init-elpy)

;; xclip - an interface to the xclip tool
;; requirements: xclip
;; (cond ((eq system-type 'gnu/linux) (require 'init-xclip))
;;       ((eq system-type 'darwin) (require 'init-osx-clipboard-mode)))
(cond ((eq system-type 'darwin) (require 'init-osx-clipboard-mode)))

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
;; (require 'init-predictive-mode)

;; La Carte
(require 'init-larcarte)

;; helm
(require 'init-helm)

;; undo tree
(require 'init-undo-tree)

;; AUCTex (LaTeX)
;; requirements: texlive-full
;; (require 'init-auctex)

;; Moz
;; (require 'init-moz)

;; zotero
;; require MozRepl for firefox: https://github.com/bard/mozrepl/wiki
;; after installing MozRepl: FireFox -> Tools -> MozRepl -> Start
;; (require 'init-zotero-minor-mode)

;; erc
(require 'init-erc)

;; ack
;; https://github.com/leoliu/ack-el
;; (require 'ack)

;; slime
;; http://www.common-lisp.net/project/slime/
;; https://github.com/slime/slime
(require 'init-slime)

;; python-django
;; https://github.com/fgallina/python-django.el
;; (require 'init-python-django)

;; inf-mongo
;; https://github.com/tobiassvn/inf-mongo
;; (require 'init-inf-mongo)

(require 'init-makefile-mode)

;; web-mode
;; http://web-mode.org/
(require 'init-web-mode)

;; languages
(require 'init-sh-mode)
(require 'init-python-mode)
(require 'init-css-mode)
(require 'init-java-mode)
(require 'init-c-mode)
(require 'init-cheetah-mode)

;; php-mode
;; https://github.com/ejmr/php-mode
(require 'init-php-mode)

;; js-mode js2-mode
;; (require 'init-js-mode)

;; go-mode
(require 'init-go-mode)
(require 'init-go-mod-mode)

;; rust-mode
(require 'init-rust-mode)

;; swift-mode
;; (require 'init-swift-mode)

;; yaml-mode
;; The emacs major mode for editing files in the YAML data serialization format.
;; https://github.com/yoshiki/yaml-mode
(require 'init-yaml-mode)

;; json-mode
;; Major mode for editing JSON files with emacs
;; https://github.com/joshwnj/json-mode
(require 'init-json-mode)

;; indent-bars
(require 'init-indent-bars)

;; dockerfile-mode
;; https://github.com/spotify/dockerfile-mode
(require 'init-dockerfile-mode)

;; csv-mode
;; http://elpa.gnu.org/packages/csv-mode.html
(require 'csv-mode)

;; ruby-mode
(require 'init-ruby-mode)

;; coffee-mode
;; https://github.com/defunkt/coffee-mode
;; (require 'init-coffee-mode)

;; nginx-mode
;; https://github.com/ajc/nginx-mode
;; (require 'init-nginx-mode)

;; Interactively resizing windows
;; Source: https://www.emacswiki.org/emacs/WindowResize
;; Not compatible with Emacs 28
;; (require 'init-iresize)

;; chinese-conv
;; A front end in emacs to convert between simplified and traditional Chinese with opencc or cconv.
;; https://github.com/gucong/emacs-chinese-conv
(require 'init-chinese-conv)

;; swagger-mode
;; Swagger Mode integrates Swagger Codegen into Emacs as a Minor Mode.
;; https://github.com/Nooby/swagger-mode
;; (require 'init-swagger-mode)

;; Smartparens
;; Minor mode for Emacs that deals with parens pairs and tries to be smart about it.
;; https://github.com/Fuco1/smartparens
;; (require 'init-smartparens)

;; company-tabnine
;; A company-mode backend for TabNine, the all-language autocompleter: https://tabnine.com/
;; https://github.com/TommyX12/company-tabnine
(require 'init-company-tabnine)

;; dumb-jump
;; an Emacs "jump to definition" package for 50+ languages
;; https://github.com/jacktasia/dumb-jump
(require 'init-dumb-jump)

;; emacs-sql-indent
;; Syntax based indentation for SQL files for GNU Emacs
;; https://github.com/alex-hhh/emacs-sql-indent
(require 'init-sql-indent)

;; emacs-hcl-mode
;; Major mode for Hashicorp Configuration Language.
;; https://github.com/purcell/emacs-hcl-mode
(require 'init-hcl-mode)

;; highlight-numbers
;; An Emacs minor mode that highlights numeric literals in source code.
;; https://github.com/Fanael/highlight-numbers
(require 'init-highlight-numbers)

;; tree-sitter
;; https://github.com/emacs-tree-sitter/elisp-tree-sitter
(require 'init-tree-sitter)
