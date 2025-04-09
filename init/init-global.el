;; Starting message
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

;; Query-replace-regexp
(global-set-key (kbd "M-#") 'query-replace-regexp)

;; Occur
(global-set-key (kbd "C-c o") 'occur)

;; Show trailing whitespace
(global-set-key (kbd "C-<f12>") 'whitespace-mode)
;; (add-hook 'prog-mode-hook 'whitespace-mode)

;; Delete trailing whitespace
(add-hook 'before-save-hook
          (lambda()
            (unless (eq major-mode 'csv-mode)
              (delete-trailing-whitespace))))

;; Use system clipboard instead of kill reagion
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

;; Inhibit startup message
(setq inhibit-startup-screen t)

;; Buffer to show after starting Emacs.
;; (setq initial-buffer-choice "~")

;; Cursor, please do not blink
(blink-cursor-mode nil)

;; Show column number in mode line
(setq column-number-mode t)

;; When point is on paranthesis, highlight the matching one
(show-paren-mode t)
(setq show-paren-style 'parenthesis
      show-paren-context-when-offscreen 'overlay)

;; Turn on highlighting current line
;; (global-hl-line-mode 1)

;; Switch windows
(global-set-key (kbd "<up>") 'windmove-up)
(global-set-key (kbd "<down>") 'windmove-down)
(global-set-key (kbd "<right>") 'windmove-right)
(global-set-key (kbd "<left>") 'windmove-left)

;; Leave point at same position in window when scrolling page
(global-set-key (kbd "M-]") 'scroll-lock-mode)

;; Daemon
;; (setq server-use-tcp t)
;; (setq-default server-socket-dir "~/.emacs.d/server")

;; Find-file default location
(find-file "~/.")

;; Backup
(setq
 backup-directory-alist
 '(("." . "~/.emacs.d/backup"))
 delete-old-versions t
 version-control t
 kept-new-versions 5
 kept-old-versions 2)

;; Default indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Insert a new line and jump to it (shift+RET)
(defun end-of-line-and-indent-new-line ()
  (interactive)
  (end-of-line)
  (electric-indent-just-newline t)
  (indent-according-to-mode))
(global-set-key (kbd "C-j") 'end-of-line-and-indent-new-line)

;; NOTE: In CSV mode, if you want to do indentation without
;; removing the trailing whitespaces, use C-j.
;;
;; (defun indent-new-line ()
;;   (interactive)
;;   (electric-indent-just-newline t)
;;   (indent-according-to-mode))
;; (global-set-key (kbd "RET") 'indent-new-line)

;; Display line number
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

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

;; Word wrap
(setq-default truncate-lines t)
(global-set-key [(control f11)] 'toggle-truncate-lines)

;; Enabling font lock
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1))

;; Visual line mode
(setq-default visual-line-mode t)
(setq-default global-visual-line-mode t)

;; Dired-mode
(setq dired-listing-switches "-alh")

;; Shells
(global-set-key [f12]
		'(lambda ()
		   (interactive)
		   (shell)))

;; Transpose lines
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

;; Original idea from
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

;; Electric pair mode
(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))
;; Don't pair quotes in electric-pair-mode
;; (setq electric-pair-inhibit-predicate
;;       (lambda (c)
;;         (if (char-equal c ?\") t (electric-pair-default-inhibit c))))
(setq electric-pair-skip-whitespace nil)

(defun inhibit-electric-pair-mode-in-minibuffer (char)
  (minibufferp))

(setq electric-pair-inhibit-predicate #'inhibit-electric-pair-mode-in-minibuffer)

;; Numbered window shortcuts
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
;; (cond
;;  ((string-equal system-type "darwin")
;;   (progn
;;     (add-to-list 'default-frame-alist '(font . "Monaco-14"))
;;     (set-fontset-font "fontset-default" 'big5' ("Heiti TC" . "unicode-bmp"))))
;;  ((string-equal system-type "gnu/linux")
;;   (progn
;;     (add-to-list 'default-frame-alist '(font . "Inconsolata-14"))))
;;  ((string-equal system-type "windows-nt")
;;   (progn
;;     (add-to-list 'default-frame-alist '(font . "Consolas-14"))
;;     (set-fontset-font "fontset-default" 'big5' ("微軟正黑體" . "unicode-bmp"))))
;;  )

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

;; Move cursor by camelCase
(global-subword-mode 1)
;; (add-hook 'prog-mode-hook 'subword-mode)

;; Switch focus after buffer split in emacs.
;; https://stackoverflow.com/questions/6464738/how-can-i-switch-focus-after-buffer-split-in-emacs
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

;; This is a workaround for the issue of large minified files of
;; programming code (i.e., code that has been compacted into the
;; smallest file size possible, which often entails removing newlines
;; should they not be strictly necessary) bringing Emacs to its knees
;; on account of the relevant modes not being (remotely) optimised for
;; that use-case. (Requires Emacs >= 27)
(global-so-long-mode 1)

;; Finding Non Ascii Characters
;; https://www.emacswiki.org/emacs/FindingNonAsciiCharacters
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

;; Keeping buffers automatically up-to-date.
(global-auto-revert-mode 1)

(provide 'init-global)
