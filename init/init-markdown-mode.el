(require 'visual-fill-column)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

(defun my/markdown-fill-paragraph-single-item (&optional justify)
  "Fill only the current sub-paragraph within a list item.
In list items, treats the first line separately from indented continuations.
When point is on a list marker line, fill only that line.
When point is on an indented continuation line, fill only the block of
indented continuations, preserving proper indentation."
  (interactive)
  (save-excursion
    (let ((orig-point (point))
          start end)
      (beginning-of-line)
      (cond
       ;; Case 1: We're on a list marker line
       ((looking-at markdown-regex-list)
        (let ((marker-indent (current-indentation))
              (marker-length (length (match-string 0))))
          (setq start (point))
          ;; Find the end of the first paragraph (before indented continuation or next list item)
          (forward-line 1)
          (while (and (not (eobp))
                      (not (looking-at markdown-regex-list))        ; Not another list item
                      (not (looking-at "^[ \t]+[^ \t\n]"))          ; Not an indented line
                      (not (looking-at "^[ \t]*$")))                ; Not a blank line
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
                   (when (looking-at markdown-regex-list)
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

       ;; Case 3: Not in a list, use default markdown fill
       (t
        (markdown-fill-paragraph justify))))))

(add-hook  'markdown-mode-hook
           (lambda ()
             ;; (whitespace-mode)
             (auto-fill-mode -1)
             (visual-line-mode nil)
             ;; (visual-line-fill-column-mode t)
             ;; (setq fill-column 120)
             ;; Use custom fill function for better list item handling
             (setq-local fill-paragraph-function #'my/markdown-fill-paragraph-single-item)
             ))

(provide 'init-markdown-mode)
