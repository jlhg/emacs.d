(add-to-list 'load-path "~/.emacs.d/package/compat")
(add-to-list 'load-path "~/.emacs.d/package/llama")
(add-to-list 'load-path "~/.emacs.d/package/with-editor/lisp")
(add-to-list 'load-path "~/.emacs.d/package/dash.el")
(add-to-list 'load-path "~/.emacs.d/package/magit/lisp")
(add-to-list 'load-path "~/.emacs.d/package/magit-popup")
(add-to-list 'load-path "~/.emacs.d/package/ghub/lisp")
(add-to-list 'load-path "~/.emacs.d/package/transient/lisp")
(add-to-list 'load-path "~/.emacs.d/package/libegit2")
(add-to-list 'load-path "~/.emacs.d/package/graphql.el")
(add-to-list 'load-path "~/.emacs.d/package/treepy.el")

(add-to-list 'load-path "~/.emacs.d/package/yaml.el")
(add-to-list 'load-path "~/.emacs.d/package/emacsql")
(add-to-list 'load-path "~/.emacs.d/package/closql")
(add-to-list 'load-path "~/.emacs.d/package/forge/lisp")
(add-to-list 'load-path "~/.emacs.d/package/cond-let")

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Make magit-status always open in current window.
;; https://github.com/magit/magit/issues/2541#issuecomment-195420321
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer
         (cond ((and (derived-mode-p 'magit-mode)
                     (eq (with-current-buffer buffer major-mode)
                         'magit-status-mode))
                nil)
               ((memq (with-current-buffer buffer major-mode)
                      '(magit-process-mode
                        magit-revision-mode
                        magit-diff-mode
                        magit-stash-mode))
                nil)
               (t
                '(display-buffer-same-window))))))

(defun my/git-commit-fill-paragraph (&optional justify)
  "Fill paragraph in git commit buffer with proper list item handling.
Treats list items like markdown-mode: first line and indented continuations separately."
  (interactive)
  (save-excursion
    (let ((orig-point (point))
          start end)
      (beginning-of-line)
      (cond
       ;; Case 1: We're on a list marker line (-, *, or +)
       ((looking-at "^[ \t]*[-*+][ \t]+")
        (let ((marker-indent (current-indentation))
              (marker-length (length (match-string 0))))
          (setq start (point))
          ;; Find the end of the first paragraph (before indented continuation or next list item)
          (forward-line 1)
          (while (and (not (eobp))
                      (not (looking-at "^[ \t]*[-*+][ \t]+"))  ; Not another list item
                      (not (looking-at "^[ \t]+[^ \t\n]"))     ; Not an indented line
                      (not (looking-at "^[ \t]*$")))            ; Not a blank line
            (forward-line 1))
          (setq end (point))
          ;; Fill with proper indentation for continuation lines
          (let ((fill-prefix (make-string (+ marker-indent marker-length) ?\s)))
            (fill-region start end justify nil))))

       ;; Case 2: We're on an indented continuation line within a list item
       ((and (looking-at "^[ \t]+[^ \t\n]")
             (save-excursion
               ;; Check if there's a list marker above
               (let ((found-marker nil))
                 (while (and (not (bobp))
                             (not found-marker))
                   (forward-line -1)
                   (when (looking-at "^[ \t]*[-*+][ \t]+")
                     (setq found-marker t)))
                 found-marker)))
        ;; Find the extent of indented continuation lines
        (let ((base-indent (current-indentation)))
          ;; Start from current line (don't search backwards)
          (setq start (point))

          ;; Find end of indented block with same or greater indentation
          (while (and (not (eobp))
                      (looking-at "^[ \t]+[^ \t\n]")
                      (>= (current-indentation) base-indent))
            (forward-line 1))
          (setq end (point))

          ;; Fill the indented region with proper hanging indent
          (let ((fill-prefix (make-string base-indent ?\s)))
            (fill-region start end justify nil))))

       ;; Case 3: Not in a list, use default fill
       (t
        (fill-paragraph justify t))))))

;; Advice log-edit-fill-entry to use our custom fill logic for list items
(defun my/git-commit-fill-entry-advice (orig-fun &rest args)
  "Advice for log-edit-fill-entry to handle list items properly."
  (if (save-excursion
        (beginning-of-line)
        (or (looking-at "^[ \t]*[-*+][ \t]+")    ; On a list marker line
            (and (looking-at "^[ \t]+[^ \t\n]")  ; On an indented line
                 (let ((found-marker nil))
                   (save-excursion
                     (while (and (not (bobp)) (not found-marker))
                       (forward-line -1)
                       (when (looking-at "^[ \t]*[-*+][ \t]+")
                         (setq found-marker t))))
                   found-marker))))
      ;; We're in a list item, use our custom fill function
      (my/git-commit-fill-paragraph)
    ;; Not in a list item, use the original function
    (apply orig-fun args)))

(advice-add 'log-edit-fill-entry :around #'my/git-commit-fill-entry-advice)

;; 50/72 formatting for Git commit message
(add-hook 'git-commit-setup-hook
          (lambda()
            (setq git-commit-summary-max-length 72)
            (auto-fill-mode t)
            (setq fill-column 72)))

(setq magit-section-visibility-indicator nil)

;; https://www.reddit.com/r/emacs/comments/bdsfb7/comment/el0lowt/?utm_source=share&utm_medium=web2x&context=3
;; https://emacs.stackexchange.com/a/52040
;; https://irreal.org/blog/?p=8877
(setq magit-section-initial-visibility-alist
      '((unpushed . show)
        (untracked .show)))

(setq transient-default-level 7)

(with-eval-after-load 'magit
  (require 'forge))

;; Fix for visiting renamed files without content changes
;; https://github.com/magit/magit/issues/2446
;; When a file is renamed without content changes, magit-diff-visit-file
;; fails because there's no hunk section to determine position.
(defun magit-diff-visit-file--handle-no-hunk (orig-fun &rest args)
  "Advice for magit-diff-visit-file--noselect to handle files without hunks.
This fixes the 'Wrong type argument: number-or-marker-p, nil' error when
visiting renamed files that have no content changes."
  (let ((toplevel (magit-toplevel)))  ; Capture toplevel in correct context
    (condition-case err
        (apply orig-fun args)
      (wrong-type-argument
       ;; If we get a wrong-type-argument error, it's likely because
       ;; there's no hunk (e.g., renamed file with no content change).
       ;; Fall back to visiting the worktree file at the beginning.
       (let* ((file-section (magit-diff--file-section))
              (file (oref file-section value))
              (full-path (expand-file-name file toplevel)))  ; Use captured toplevel
         (list (find-file-noselect full-path) nil))))))

(advice-add 'magit-diff-visit-file--noselect
            :around #'magit-diff-visit-file--handle-no-hunk)

(provide 'init-magit)
