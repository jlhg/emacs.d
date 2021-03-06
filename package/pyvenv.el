;;; pyvenv.el --- Python virtual environment interface -*- lexical-binding: t -*-

;; Copyright (C) 2013  Jorgen Schaefer <contact@jorgenschaefer.de>

;; Author: Jorgen Schaefer <contact@jorgenschaefer.de>
;; URL: http://github.com/jorgenschaefer/pyvenv
;; Version: 1.0

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple global minor mode which will replicate the changes
;; done by virtualenv activation inside Emacs.

;; The main entry points are `pyvenv-activate', which queries the user
;; for a virtual environment directory to activate, and
;; `pyvenv-workon', which queries for a virtual environment in
;; $WORKON_HOME (from virtualenvwrapper.sh).

;; If you want your inferior Python processes to be restarted
;; automatically when you switch your virtual environment, add
;; `pyvenv-restart-python' to `pyvenv-post-activate-hooks'.

;;; Code:

(require 'json)

;; API for other libraries or user customization.

(defvar pyvenv-virtual-env nil
  "The current virtual environment.

Do not set this variable directly; use `pyvenv-activate' or
`pyvenv-workon'.")

(defvar pyvenv-virtual-env-name nil
  "The name of the current virtual environment.

This is usually the base name of `pyvenv-virtual-env'.")

(defvar pyvenv-pre-activate-hooks nil
  "Hooks run before a virtual environment is activated.

`pyvenv-virtual-env' is already set.")

(defvar pyvenv-post-activate-hooks nil
  "Hooks run after a virtual environment is activated.

`pyvenv-virtual-env' is set.")

(defvar pyvenv-pre-deactivate-hooks nil
  "Hooks run before a virtual environment is deactivated.

`pyvenv-virtual-env' is set.")

(defvar pyvenv-post-deactivate-hooks nil
  "Hooks run after a virtual environment is deactivated.

`pyvenv-virtual-env' is still set.")

(defvar pyvenv-workon nil
  "A variable requesting a specific virtualenv.

This is meant to be set in file- or directory-local variables.

When `pyvenv-mode' is enabled, pyvenv will switch to this
virtualenv. If a virtualenv is already enabled, it will ask first.")

(defgroup pyvenv nil
  "Python Virtual Environment Interface."
  :prefix "pyvenv-"
  :group 'languages)

(defcustom pyvenv-mode-line-indicator '(pyvenv-virtual-env-name
                                        ("[" pyvenv-virtual-env-name "] "))
  "How `pyvenv-mode' will indicate the current environment in the mode line."
  :group 'pyvenv)

;; Internal code.

(defvar pyvenv-old-process-environment nil
  "The old process environment before the last activate.")

(defvar pyvenv-old-exec-path nil
  "The old exec path before the last activate.")

;;;###autoload
(defun pyvenv-activate (directory)
  "Activate the virtual environment in DIRECTORY."
  (interactive "DActivate venv: ")
  (setq directory (expand-file-name directory))
  (pyvenv-deactivate)
  (setq pyvenv-virtual-env directory
        pyvenv-virtual-env-name (file-name-base directory))
  ;; Preserve variables from being overwritten.
  (let ((old-exec-path exec-path)
        (old-process-environment process-environment))
    (unwind-protect
        (pyvenv-run-virtualenvwrapper-hook "pre_activate" pyvenv-virtual-env)
      (setq exec-path old-exec-path
            process-environment old-process-environment)))
  (run-hooks 'pyvenv-pre-activate-hooks)
  (setq pyvenv-old-exec-path exec-path
        pyvenv-old-process-environment process-environment
        ;; For some reason, Emacs adds some directories to `exec-path'
        ;; but not to `process-environment'?
        exec-path (cons (format "%s/bin" directory)
                        exec-path)
        process-environment (append
                             (list
                              (format "VIRTUAL_ENV=%s" directory)
                              (format "PATH=%s" (mapconcat (lambda (x)
                                                             (or x "."))
                                                           exec-path
                                                           ":"))
                              ;; No "=" means to unset
                              "PYTHONHOME")
                             process-environment)
        )
  (pyvenv-run-virtualenvwrapper-hook "post_activate")
  (run-hooks 'pyvenv-post-activate-hooks))

;;;###autoload
(defun pyvenv-deactivate ()
  "Deactivate any current virtual environment."
  (interactive)
  (when pyvenv-virtual-env
    (pyvenv-run-virtualenvwrapper-hook "pre_deactivate")
    (run-hooks 'pyvenv-pre-deactivate-hooks))
  (when pyvenv-old-process-environment
    (setq process-environment pyvenv-old-process-environment
          pyvenv-old-process-environment nil))
  (when pyvenv-old-exec-path
    (setq exec-path pyvenv-old-exec-path
          pyvenv-old-exec-path nil))
  (when pyvenv-virtual-env
    ;; Make sure this does not change `exec-path', as $PATH is
    ;; different
    (let ((old-exec-path exec-path)
          (old-process-environment process-environment))
      (unwind-protect
          (pyvenv-run-virtualenvwrapper-hook "post_deactivate"
                                             pyvenv-virtual-env)
        (setq exec-path old-exec-path
              process-environment old-process-environment)))
    (run-hooks 'pyvenv-post-deactivate-hooks))
  (setq pyvenv-virtual-env nil
        pyvenv-virtual-env-name nil))

(defvar pyvenv-workon-history nil
  "Prompt history for `pyvenv-workon'.")

;;;###autoload
(defun pyvenv-workon (name)
  "Activate a virtual environment from $WORKON_HOME."
  (interactive
   (list
    (completing-read "Work on: " (pyvenv-virtualenv-list)
                     nil t nil 'pyvenv-workon-history nil nil)))
  (when (not (or (equal name "")
                 ;; Some completion frameworks can return nil for the
                 ;; default, see
                 ;; https://github.com/jorgenschaefer/elpy/issues/144
                 (equal name nil)))
    (pyvenv-activate (format "%s/%s"
                             (or (getenv "WORKON_HOME")
                                 "~/.virtualenvs")
                             name))))

(defun pyvenv-virtualenv-list ()
  "Prompt the user for a name in $WORKON_HOME."
  (let ((workon-home (or (getenv "WORKON_HOME")
                         "~/.virtualenvs"))
        (result nil))
    (when (not (file-directory-p workon-home))
      (error "Can't find a workon home directory, set $WORKON_HOME"))
    (dolist (name (directory-files workon-home))
      (when (file-exists-p (format "%s/%s/bin/activate"
                                   workon-home name))
        (setq result (cons name result))))
    (sort result (lambda (a b)
                   (string-lessp (downcase a)
                                 (downcase b))))))

;;;###autoload
(define-minor-mode pyvenv-mode
  "Global minor mode for pyvenv.

Will show the current virtual env in the mode line, and respect a
`pyvenv-workon' setting in files."
  :global t
  (cond
   (pyvenv-mode
    (add-to-list 'mode-line-misc-info pyvenv-mode-line-indicator)
    (add-hook 'python-mode-hook 'pyvenv-set-file-virtualenv))
   ((not pyvenv-mode)
    (setq mode-line-misc-info (delete pyvenv-mode-line-indicator
                                      mode-line-misc-info))
    (remove-hook 'python-mode-hook 'pyvenv-set-file-virtualenv))))

(defun pyvenv-set-file-virtualenv ()
  "If `pyvenv-workon' is set, switch to that virtual env."
  (cond
   ((and pyvenv-workon (not pyvenv-virtual-env))
    (pyvenv-workon pyvenv-workon))
   ((and pyvenv-workon (not (equal pyvenv-workon pyvenv-virtual-env)))
    (when (y-or-n-p (format "Switch to virtual env %s (currently %s)? "
                            pyvenv-workon pyvenv-virtual-env))
      (pyvenv-workon pyvenv-workon)))))

(defvar pyvenv-virtualenvwrapper-python
  (or (getenv "VIRTUALENVWRAPPER_PYTHON")
      (executable-find "python"))
  "The python process which has access to the virtualenvwrapper module.

This should be $VIRTUALENVWRAPPER_PYTHON outside of Emacs, but
virtualenvwrapper.sh does not export that variable, so we do not
usually see it.")

(defun pyvenv-run-virtualenvwrapper-hook (hook &rest args)
  "Run a virtualenvwrapper hook, and update the environment.

This will run a virtualenvwrapper hook and update the local
environment accordingly.

CAREFUL! This will modify your `process-environment' and
`exec-path'."
  (when (getenv "VIRTUALENVWRAPPER_LOG_DIR")
    (with-temp-buffer
      (let ((tmpfile (make-temp-file "pyvenv-virtualenvwrapper-")))
        (unwind-protect
            (progn
              (apply #'call-process
                     pyvenv-virtualenvwrapper-python
                     nil t nil
                     "-c"
                     "from virtualenvwrapper.hook_loader import main; main()"
                     "--script" tmpfile
                     (if (getenv "HOOK_VERBOSE_OPTION")
                         (cons (getenv "HOOK_VERBOSE_OPTION")
                               (cons hook args))
                       (cons hook args)))
              (call-process-shell-command
               (format ". '%s' ; echo ; echo =-=-= ; python -c \"import os, json ; print json.dumps(dict(os.environ))\""
                       tmpfile)
               nil t nil))
          (delete-file tmpfile)))
      (goto-char (point-min))
      (when (re-search-forward "\n=-=-=\n" nil t)
        (let ((output (buffer-substring (point-min)
                                        (match-beginning 0))))
          (when (> (length output) 0)
            (with-help-window "*Virtualenvwrapper Hook Output*"
              (with-current-buffer "*Virtualenvwrapper Hook Output*"
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert
                   (format
                    "Output from the virtualenvwrapper hook %s:\n\n"
                    hook)
                   output))))))
        (dolist (binding (json-read))
          (let ((env (format "%s=%s" (car binding) (cdr binding))))
            (when (not (member env process-environment))
              (setq process-environment (cons env process-environment))))
          (when (eq (car binding) 'PATH)
            (setq exec-path (split-string (cdr binding) ":"))))))))

;;;###autoload
(defun pyvenv-restart-python ()
  "Restart Python inferior processes."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (eq major-mode 'inferior-python-mode)
                 (get-buffer-process buf))
        (let ((cmd (combine-and-quote-strings (process-command
                                               (get-buffer-process buf))))
              (dedicated (if (string-match "\\[.*\\]$" (buffer-name buf))
                             t
                           nil))
              (show nil))
          (delete-process (get-buffer-process buf))
          (goto-char (point-max))
          (insert "\n\n"
                  "###\n"
                  (format "### Restarting in virtual env %s (%s)\n"
                          pyvenv-virtual-env-name pyvenv-virtual-env)
                  "###\n"
                  "\n\n")
          (run-python cmd dedicated show)
          (goto-char (point-max)))))))

;;; Compatibility

(when (not (fboundp 'file-name-base))
  ;; Emacs 24.3
  (defun file-name-base (&optional filename)
    "Return the base name of the FILENAME: no directory, no extension.
FILENAME defaults to `buffer-file-name'."
    (file-name-sans-extension
     (file-name-nondirectory (or filename (buffer-file-name)))))
  )

(provide 'pyvenv)
;;; pyvenv.el ends here
