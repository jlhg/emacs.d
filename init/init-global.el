;; color theme
;; (add-to-list 'load-path "~/.emacs.d/package/color-theme")
;; (require 'color-theme)
;; (require 'color-theme-customized)
;; (color-theme-initialize)
;; (color-theme-customized)
;; (color-theme-select)
;; (color-theme-comidia)

;; customized faces for python mode
;; (font-lock-add-keywords 'python-mode
;;                         '(;;("\\<\\(object\\|str\\|else\\|except\\|finally\\|try\\|\\)\\>" 0 'py-builtins-face)  ; adds object and str and fixes it so that keywords that often appear with : are assigned as builtin-face
;;                           ;; ("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-defaults) ; FIXME: negative or positive prefixes do not highlight to this regexp but does to one below
;;                           ("\\([][{}()~^<>:=,.\\+*/%-]\\)" 0 'font-lock-type-face)))

;; Zenburn-theme
;; https://github.com/bbatsov/zenburn-emacs
(require 'zenburn-theme)
(load-theme 'zenburn t)

;; starting message
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

;; query-replace-regexp
(global-set-key (kbd "M-#") 'query-replace-regexp)

;; occur
(global-set-key (kbd "C-c o") 'occur)

;; show trailing whitespace
(global-set-key (kbd "C-<f12>") 'whitespace-mode)

;; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; use system clipboard instead of kill reagion
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

;; inhibit startup message
(setq inhibit-startup-screen t)

;; buffer to show after starting Emacs.
;; (setq initial-buffer-choice "~")

;; cursor, please do not blink
(blink-cursor-mode nil)

;; show column number in mode line
(setq column-number-mode t)

;; when point is on paranthesis, highlight the matching one
(show-paren-mode t)

;; turn on highlighting current line
;; (global-hl-line-mode 1)

;; switch windows
(global-set-key (kbd "M-E") 'windmove-up)
(global-set-key (kbd "M-D") 'windmove-down)
(global-set-key (kbd "M-F") 'windmove-right)
(global-set-key (kbd "M-S") 'windmove-left)

;; leave point at same position in window when scrolling page
(global-set-key (kbd "M-]") 'scroll-lock-mode)

;; daemon
;; (setq server-use-tcp t)
;; (setq-default server-socket-dir "~/.emacs.d/server")

;; find-file default location
(find-file "~/.")

;; backup
(setq
 backup-directory-alist
 '(("." . "~/.emacs.d/backup"))
 delete-old-versions t
 version-control t
 kept-new-versions 5
 kept-old-versions 2)

;; default indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default newline-and-indent t)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; display line number
(global-linum-mode t)
(setq linum-format "%3d ")

;; Do not turn on linum-mode in some major modes (and file with over 2000 lines)
;; due to the performance issue.
;; https://github.com/kuanyui/.emacs.d/blob/master/rc/rc-basic.el#L203-L233
(setq inhibit-linum-mode-alist
      `(eshell-mode
        shell-mode
        term-mode
        erc-mode
        compilation-mode
        woman-mode
        w3m-mode
        magit-mode
        magit-status-mode
        org-mode
        ,(if (not (window-system)) 'twittering-mode)
        ))

(defadvice linum-on (around inhibit-for-modes activate)
  "Stop turing linum-mode if it is in the inhibit-linum-mode-alist."
  (unless (or (member major-mode inhibit-linum-mode-alist)
              (and (eq major-mode 'org-mode)
                   (> (count-lines (point-min) (point-max)) 2000)))
    ad-do-it))

;; word wrap
(setq-default truncate-lines t)
(global-set-key [(control f11)] 'toggle-truncate-lines)

;; enabling font lock
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1))

;; visual line mode
(setq-default visual-line-mode t)
(setq-default global-visual-line-mode t)

;; Dired-mode
(setq dired-listing-switches "-alh")

;; Shells
(global-set-key [f12]
		'(lambda ()
		   (interactive)
		   (shell)))

;; transpose lines
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))
(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))
(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; insert a new line and jump to it (shift+RET)
(defun end-of-line-and-indent-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "C-j") 'end-of-line-and-indent-new-line)

;; original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key (kbd "M-;") 'comment-dwim-line)

;; electric pair mode
(when (fboundp 'electric-pair-mode)
  (electric-pair-mode)
  (setq electric-pair-skip-self nil)
  (setq electric-pair-preserve-balance nil))

;; numbered window shortcuts
;; use M-1 through M-0 to navigate
(require 'window-numbering)
(window-numbering-mode t)

;; Smex is a M-x enhancement for Emacs. Built on top of Ido,
;; it provides a convenient interface to your recently and most
;; frequently used commands. And to all the other commands, too.
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Accept UTF-8 (uppercase) encoding
(define-coding-system-alias 'UTF-8 'utf-8)

;; Previous buffer
(global-set-key (kbd "C-c p") 'previous-buffer)

;; Next buffer
(global-set-key (kbd "C-c n") 'next-buffer)

;; GUI settings
(if (fboundp 'menu-bar-mode)
        (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
         (tool-bar-mode -1))
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode 0))

;; Font settings
(cond
 ((string-equal system-type "darwin")
  (progn
    (add-to-list 'default-frame-alist '(font . "Monaco-14"))
    (set-fontset-font "fontset-default" 'big5' ("Heiti TC" . "unicode-bmp"))))
 ((string-equal system-type "gnu/linux")
  (progn
    (add-to-list 'default-frame-alist '(font . "Inconsolata-14"))))
 ((string-equal system-type "windows-nt")
  (progn
    (add-to-list 'default-frame-alist '(font . "Consolas-14"))
    (set-fontset-font "fontset-default" 'big5' ("微軟正黑體" . "unicode-bmp"))))
 )

;; Launch emacsclient maximized from the commandline
;; 1. $ emacsclient -nc -F "((fullscreen . maximized))"
;; 2. $ emacsclient -nc -F "((fullscreen . fullboth))"
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Set the frame title in the titlebar
(setq frame-title-format (list "Emacs - " (getenv "USERNAME") "@" system-name ": "
                               '(buffer-file-name
                                 "%f"
                                 (dired-directory dired-directory "%b"))))

(desktop-save-mode 1)

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable backslash character in line wrapping
(set-display-table-slot standard-display-table 'wrap ?\ )

(provide 'init-global)
