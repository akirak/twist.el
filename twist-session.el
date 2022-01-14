;;; twist-session.el --- A REPL session for inspecting Twist configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/twist.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides support for interacting with a nix repl session for
;; inspecting a Twist configuration package.

;;; Code:

(require 'nix)
(require 'comint)
(require 'map)

;;;; Foundation

;; Based on https://emacs.stackexchange.com/a/18884
(defconst twist-session-ansi-escape-re
  (rx (or ?\233 (and ?\e ?\[))
      (zero-or-more (char (?0 . ?\?)))
      (zero-or-more (char ?\s ?- ?\/))
      (char (?@ . ?~))))

(defconst twist-session-eval-buffer "*twist eval*")
(defconst twist-session-build-buffer "*twist build*")

(defcustom twist-session-response-timeout 500
  "Default timeout from nix repl in milliseconds."
  :group 'twist
  :type 'number)

(defvar twist-session-system nil)
(defvar twist-session-buffer nil)

(defvar-local twist-session-flake nil)
(defvar-local twist-session-eval-root nil)
(defvar-local twist-session-loaded-time nil)

(defun twist-session--make-eval-root (system package)
  (format "outputs.packages.%s.%s"
          (or system twist-session-system)
          package))

(defun twist-session-process-live-p ()
  "Return non-nil if twist-session-buffer has a live process."
  (let ((process (when (bufferp twist-session-buffer)
                   (get-buffer-process twist-session-buffer))))
    (when process
      (process-live-p process))))

(defun twist-session-directory ()
  "Return `default-directory' of the current/last session."
  (buffer-local-value 'default-directory twist-session-buffer))

(defun twist-session-flake ()
  "Return the flake of the current/last session."
  (buffer-local-value 'twist-session-flake twist-session-buffer))

(defun twist-session-config-package ()
  "Return the package name of the current/last session."
  (twist-session--last-attr
   (buffer-local-value 'twist-session-eval-root twist-session-buffer)))

(defun twist-session--last-attr (attr-path)
  (save-match-data
    (string-match (rx (+ (not (any "."))) eol) attr-path)
    (match-string 0 attr-path)))

(defun twist-session-status ()
  "Display the status of the current session."
  (interactive)
  (if (and twist-session-buffer
           (bufferp twist-session-buffer))
      (let ((proc (get-buffer-process twist-session-buffer)))
        (message "%s#%s, %s"
                 (twist-session-flake)
                 (twist-session-config-package)
                 (if (and proc (process-live-p proc))
                     "active"
                   "inactive")))
    (message "No buffer")))

(defun twist-session-reopen ()
  "Re-open the previously running session."
  (cond
   ((twist-session-process-live-p)
    (user-error "There is a live session of Twist"))
   (twist-session-buffer
    (twist-session-open (twist-session-flake)
                        (twist-session-config-package)))
   (t
    (user-error "No information on the previous session is available"))))

(defmacro twist-session-ensure (&rest progn)
  `(if (twist-session-process-live-p)
       (progn
         ,@progn)
     (user-error "First start a session using `twist-connect'")))

(defmacro twist-session--with-live-buffer (reload &rest progn)
  "Evaluate PROGN on the comint buffer."
  (declare (indent 1))
  `(if (twist-session-process-live-p)
       (with-current-buffer twist-session-buffer
         (when ,reload
           (twist-session--reload-flake))
         ,@progn)
     (user-error "Twist session is closed or the process is dead")))

(defun twist-session-open (&optional flake package)
  "Open a new session that inspects a FLAKE PACKAGE of twist."
  (when (twist-session-process-live-p)
    (user-error "There is a live session of Twist"))
  (let* ((dir (cond
               ((file-directory-p flake)
                flake)
               (flake
                nil)
               (t
                (read-directory-name "Flake: "))))
         (flake (when dir
                  (twist-flake-normalize-path dir)))
         (buffer (make-comint-in-buffer "Twist-Repl"
                                        (get-buffer-create "*twist repl*")
                                        nix-executable nil "repl")))
    (setq twist-session-buffer buffer)
    (add-hook 'kill-emacs-hook #'twist-session-cleanup)
    (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
    (with-current-buffer buffer
      (when dir
        (setq default-directory (file-name-as-directory flake)))
      (erase-buffer)
      ;; Just in case the previous data exists
      (twist-session--clear)
      ;; Read initial output
      (twist-session--accept-output)

      (twist-session--load-flake flake)

      (let* ((system (or twist-session-system
                         (setq twist-session-system (nix-system))))
             (package (or (and flake package)
                          (completing-read "Package: "
                                           (twist-session--eval-complex
                                            (format "builtins.attrNames outputs.packages.%s"
                                                    system))))))
        (setq-local twist-session-eval-root (twist-session--make-eval-root
                                             system package))))
    buffer))

(defun twist-session--load-flake (flake)
  "Load FLAKE into the current repl session."
  ;; I tried to use builtins.getFlake to support remote repositories, but
  ;; somehow it failed with the following error. For now, I will support
  ;; only local flakes:
  ;;
  ;; *** Eval error ***  JSON readtable error: 101
  (twist-session--send-command (concat ":lf " flake))

  (prog1 (setq-local twist-session-loaded-time (current-time)
                     twist-session-flake flake)
    (message "Loaded a flake from %s for Twist" flake)))

(defun twist-session--reload-flake ()
  "Reload the current flake."
  (twist-session--clear)
  (twist-session--send-command ":r")
  (twist-session--load-flake twist-session-flake))

(defun twist-session-reload ()
  (twist-session--with-live-buffer t))

(defun twist-session--send-command (command &optional timeout)
  "Send a command to the nix repl session and ignore its output."
  (insert command)
  (comint-send-input)
  (twist-session--accept-output timeout))

(defun twist-session--eval (input &optional timeout)
  "Evaluate INPUT and return its result, with optional TIMEOUT."
  (with-current-buffer (get-buffer-create twist-session-eval-buffer)
    ;; This is extremely important because the point is moved to the start every
    ;; time.
    (erase-buffer))
  ;; `comint-redirect-send-command' seems to be flakey. I don't know why, but
  ;; using `comint-redirect-setup' seem to have solved the issue.
  (comint-redirect-setup twist-session-eval-buffer
                         (current-buffer)
                         "^nix-repl> ")
  (add-function :around (process-filter (get-buffer-process (current-buffer)))
                #'comint-redirect-filter)
  (process-send-string (current-buffer) (concat input "\n"))
  (twist-session--accept-output timeout)
  (with-current-buffer twist-session-eval-buffer
    (goto-char (point-min))

    (save-excursion
      ;; If the input is long, it will be wrapped by ^H, which breaks
      ;; `search-forward'. Erase them.
      (while (re-search-forward (rx (* space) "") nil t)
        (replace-match "")))

    (save-excursion
      ;; Every output is wrapped in escape sequences for highlighting.
      ;; I only need the data, so replace them as well.
      (while (re-search-forward twist-session-ansi-escape-re nil t)
        (replace-match "")))

    ;; nix-repl echoes every user input to the terminal. Find the end of the
    ;; input and move the point to right after the echoed input.
    (search-forward input)
    ;; Skip whitespaces
    (when (looking-at (rx (+ space)))
      (goto-char (match-end 0)))

    ;; Every response is printed as a Nix string wrapped in double quotes.
    ;; Decode them as a JSON string.
    (json-read)))

(defun twist-session--eval-complex (expression &optional timeout
                                               &rest json-args)
  "Read a complex expression from the repl session.

EXPRESSION is a string that should be evaluated in the Nix repl
session.

If TIMEOUT is not set, `twist-session-response-timeout' is used by default.

Optionally, you can override arguments passed to
`json-parse-string' with JSON-ARGS."
  ;; If the expected response is not a primitive data (e.g. arrays and attr
  ;; sets), make Nix convert it to JSON and parse it. Emacs has a native JSON
  ;; parser since version 27, so it should be fast.
  (apply #'json-parse-string
         (twist-session--eval (format "builtins.toJSON (%s)" expression) timeout)
         (append json-args
                 (list :object-type 'alist
                       :array-type 'list
                       :null-object nil
                       :false-object nil))))

(defun twist-session--accept-output (&optional timeout)
  "Read output from the process with TIMEOUT."
  (while (accept-process-output (get-buffer-process (current-buffer))
                                nil
                                (or timeout twist-session-response-timeout))))

;;;; APIs

(defmacro twist-session--with-eval-msg (&rest progn)
  "Print a message while evaluating PROGN."
  (declare (indent 0))
  `(progn
     (let ((message-log-max nil))
       (message "Evaluating the data..."))
     (prog1 (progn ,@progn)
       (message nil))))

(defvar twist-session-packages nil)
(defun twist-session-packages (&optional refresh)
  "Return a hash table of packages in the configuration."
  (or twist-session-packages
      (let ((alist (twist-session--with-eval-msg
                     (twist-session--with-live-buffer refresh
                       (twist-session--eval-complex
                        (format "%s.packageInputs"
                                twist-session-eval-root)
                        2500))))
            ;; TODO: Fine-tune hash parameters
            (hash (make-hash-table :test #'equal :size 100)))
        (pcase-dolist (`(,symbol . ,data) alist)
          (puthash (symbol-name symbol) data hash))
        (setq twist-session-packages hash)
        alist)))

(defun twist-session-package (ename)
  "Return a list of elisp packages in the configuration."
  (gethash (cl-etypecase ename
             (string ename)
             (symbol (symbol-name ename)))
           (twist-session-packages)))

(defun twist-session-reverse-deps (ename)
  (let ((ename (cl-etypecase ename
                 (string (intern ename))
                 (symbol ename))))
    (thread-last (twist-session-packages)
      (map-filter (lambda (_ename data)
                    (thread-last data
                      (assq 'packageRequires)
                      (cdr)
                      (assq ename))))
      (map-keys))))

(defun twist-session-package-outputs (ename)
  "Return the outputs of a package derivation with ENAME."
  ;; There is no cache, so omit twist-session--with-eval-msg for silence.
  (twist-session--with-live-buffer nil
    (twist-session--eval-complex
     (format "%s.elispPackages.%s.outputs"
             twist-session-eval-root ename)
     100)))

(defvar twist-session-background-task nil)

(define-error 'twist-session-build-error "Package failed to build"
  'twist-session-build-errors)

(defun twist-session-build-package (ename)
  (declare (indent 1))
  (if twist-session-background-task
      (user-error "Build in progress")
    (with-current-buffer (get-buffer-create twist-session-build-buffer)
      (erase-buffer))
    (twist-session--with-live-buffer nil
      (comint-redirect-setup twist-session-build-buffer
                             (current-buffer)
                             "^nix-repl> ")
      (add-function :around (process-filter (get-buffer-process (current-buffer)))
                    #'comint-redirect-filter)
      (process-send-string (current-buffer)
                           (format ":b %s.elispPackages.%s\n"
                                   twist-session-eval-root ename))
      (while (not comint-redirect-completed)
        (twist-session--accept-output 50)))
    (let (result)
      (with-current-buffer twist-session-build-buffer
        (goto-char (point-min))
        (save-match-data
          (while (re-search-forward (rx bol (* space) (group (+ (any alpha)))
                                        (+ space) "->" (+ space)
                                        (group "/" (+ (not (any space)))))
                                    nil t)
            (push (cons (match-string 1)
                        (match-string 2))
                  result)))
        (when (and (not result)
                   (re-search-forward (rx bol (* space) "error: builder for") nil t))
          (signal 'twist-session-build-error (list ename)))
        result))))

;;;; Clear cache data

(defun twist-session--clear ()
  "Clear data stored in buffer-local variables."
  (when twist-session-packages
    (clrhash twist-session-packages)
    (setq twist-session-packages nil)))

;;;; Finalization

(defun twist-session-close (&optional no-error)
  "Finish the current twist session."
  (if (twist-session-process-live-p)
      (with-current-buffer twist-session-buffer
        (twist-session--send-command ":q")
        (twist-session--clear)
        (message "Finished the twist session"))
    (unless no-error
      (user-error "Not connected"))))

(defun twist-session-cleanup ()
  "Finish the currently running session if it is active."
  (twist-session-close t))

(provide 'twist-session)
;;; twist-session.el ends here
