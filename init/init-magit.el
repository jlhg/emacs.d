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
