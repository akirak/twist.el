;;; twist-package.el --- Show package information in a buffer -*- lexical-binding: t -*-

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

;; This library provides an interface that displays information on a package in
;; the configuration of twist.

;;; Code:

(require 'twist-session)
(require 'twist-bookmark)

(require 'magit-section)
(require 'elx)

(declare-function epkg-list-packages-by-author "ext:epkg")
(defvar thing-at-point-email-regexp)
(defvar bookmark-make-record-function)

(defgroup twist-package nil
  "Show package information in a buffer."
  :group 'twist)

;;;; Constants

(defmacro twist-package-make-regexp (&rest basenames)
  `(rx (or ,@basenames)
       (?  "." (+ anything))))

;; case-fold-search doesn't seem to take effect on directory-files

(defconst twist-package-license-regexp
  (twist-package-make-regexp "LICENSE" "UNLICENSE" "license" "COPYING" "copying"))

(defconst twist-package-readme-regexp
  (twist-package-make-regexp "README" "readme"))

(defconst twist-package-contributing-regexp
  (twist-package-make-regexp "CONTRIBUTING"))

(defconst twist-package-community-regexp
  (rx (or (regexp twist-package-license-regexp)
          (regexp twist-package-readme-regexp)
          (regexp twist-package-contributing-regexp))))

;;;; Custom variables

(defcustom twist-package-sections
  '(twist-package-header-section
    twist-package-documents-section
    twist-package-metadata-section
    twist-package-authors-section
    twist-package-maintainers-section
    twist-package-license-section
    twist-package-deps-section
    twist-package-lisp-files-section
    twist-package-other-files-section
    twist-package-reverse-deps-section)
  "A list of functions that insert contents in `twist-package-mode'.

Each function should take an argument which is a tree containing
information of a package."
  :group 'twist
  :type 'hook
  :options '(twist-package-header-section
             twist-package-documents-section
             twist-package-metadata-section
             twist-package-authors-section
             twist-package-maintainers-section
             twist-package-license-section
             twist-package-deps-section
             twist-package-lisp-files-section
             twist-package-other-files-section
             twist-package-reverse-deps-section))

(defcustom twist-package-do-build t
  "Whether to build the package when a package is displayed."
  :type 'boolean)

;;;; Faces

(defface twist-package-title-face
  '((t :height 1.4))
  "Face for package names."
  :group 'twist-package)

(defface twist-package-summary-face
  '((t :italic t))
  "Face for package summaries."
  :group 'twist-package)

(defface twist-package-known-face
  '((t))
  "Face for known keywords."
  :group 'twist-package)

(defface twist-package-unknown-face
  '((((background light) (class color)) :foreground "DimGrey")
    (((background dark) (class color)) :foreground "DarkGray"))
  "Face for unknown keywords."
  :group 'twist-package)

;;;; Local variables

(defvar-local twist-package-ename nil
  "Name of the package described in the buffer.")

(defvar-local twist-package-outputs nil
  "Outputs of the package in the buffer.")

(defvar-local twist-package-build-status nil
  "Build status of the package.")

;;;; Button types
(define-button-type 'twist-package-url
  :supertype 'help-xref
  'help-function 'browse-url
  'help-echo (purecopy "mouse-2, RET: Browse the URL"))

(define-button-type 'twist-package-license
  :supertype 'help-xref
  'help-function 'twist-package-find-license-file
  'help-echo (purecopy "mouse-2, RET: Find a license file"))

(define-button-type 'twist-package-store-path
  :supertype 'help-xref
  ;; TODO: Make customizable
  'help-function 'dired
  'help-echo (purecopy "mouse-2, RET: Browse the directory"))

(define-button-type 'twist-package-recipe
  :supertype 'help-xref
  'help-function 'twist-package-find-file
  'help-echo (purecopy "mouse-2, RET: Browse the recipe"))

(define-button-type 'twist-package-source-file
  :supertype 'help-xref
  'help-function 'twist-package-find-file
  'help-echo (purecopy "mouse-2, RET: Find the file in the store"))

(define-button-type 'twist-package-name
  :supertype 'help-xref
  'help-function (lambda (ename)
                   (switch-to-buffer (twist-package-buffer ename)))
  'help-echo (purecopy "mouse-2, RET: Describe the package"))

(define-button-type 'twist-package-author
  :supertype 'help-xref
  'help-function 'twist-package-discover-author
  'help-echo (purecopy "mouse-2, RET: Discover more packages by the author"))

(define-button-type 'twist-package-output
  :supertype 'help-xref
  'help-function 'twist-package-find-file-in-output
  'help-echo (purecopy "mouse-2, RET: Browse the file in the output"))

(define-button-type 'twist-package-output-dir
  :supertype 'help-xref
  'help-function 'twist-package-browse-output-dir
  'help-echo (purecopy "mouse-2, RET: Browse the output directory"))

(define-button-type 'twist-package-build
  :supertype 'help-xref
  'help-function 'twist-package-build
  'help-echo (purecopy "mouse-2, RET: Build the package"))

;;;; Package buffers

;;;###autoload
(defun twist-package (package)
  "Describe a PACKAGE."
  (interactive (list (completing-read "Describe package: "
                                      (twist-session-packages))))
  (twist-session-ensure
   (switch-to-buffer (twist-package-buffer package))))

(defun twist-package-buffer (ename)
  (let* ((ename (cl-etypecase ename
                  (string (intern ename))
                  (symbol ename)))
         ;; There may be a dependency in Package-Requires header which is not
         ;; package (e.g. built-in library), so the existence must be checked
         ;; beforehand.
         (data (twist-session-package ename))
         (default-directory (twist-session-directory)))
    (if data
        (with-current-buffer (get-buffer-create (format "*Twist<%s>*" ename))
          (twist-package-mode)
          (setq twist-package-ename ename)
          (twist-package--insert-contents data)
          (goto-char (point-min))
          (setq-local bookmark-make-record-function #'twist-package-bookmark-record)
          (current-buffer))
      (user-error "Library %s is not a package in the configuration" ename))))

(defun twist-package-revert (&rest _args)
  (save-excursion
    (twist-package--insert-contents (twist-session-package twist-package-ename))))

(defun twist-package--insert-contents (data)
  (let ((inhibit-read-only t))
    (erase-buffer)

    (when twist-package-do-build
      (unless twist-package-outputs
        (condition-case-unless-debug _
            (when (setq twist-package-outputs
                        (with-timeout (0.3)
                          (twist-session-build-package (alist-get 'ename data) #'identity)))
              (setq twist-package-build-status 'success))
          (error (setq twist-package-build-status 'failed)))))

    (magit-insert-section ('package)
      (run-hook-with-args 'twist-package-sections data))

    (when (eq twist-package-build-status 'failed)
      (message "Package failed to build"))))

(define-derived-mode twist-package-mode magit-section-mode
  "Twist Package"
  (setq-local revert-buffer-function #'twist-package-revert)
  ;; (setq-local cursor-type nil)
  (read-only-mode 1))

;;;; Sections

;;;;; Utilities

(defmacro twist-package-lookup (path data)
  "Look up PATH in a tree DATA.

This is a helper macro for traversing a tree."
  (let ((path (if (listp path)
                  path
                (list path))))
    (cl-reduce (lambda (acc f)
                 `(cdr (assq ',f ,acc)))
               (cdr path)
               :initial-value
               `(cdr (assq ',(car path) ,data)))))

(defsubst twist-package--to-list (obj)
  "If OBJ is not a list, turn it into a singleton list."
  (cond
   ((listp obj)
    obj)
   (obj
    (list obj))
   (t
    nil)))

(defun twist-package--insert-identity (identity)
  (pcase identity
    ((rx bol
         (group (+ (not (any "<"))))
         (+ space)
         "<" (group (+ (not (any ">")))) ">"
         eol)
     (let ((author (match-string 1 identity))
           (email (match-string 2 identity)))
       (insert-text-button author
                           'type 'twist-package-author
                           'help-args (list author))
       (insert " <")
       (insert-text-button email
                           'type 'twist-package-author
                           'help-args (list email))
       (insert ">")))
    (_
     (insert-text-button identity
                         'type 'twist-package-author
                         'help-args (list identity)))))

(defmacro twist-package--people-section (name heading identities)
  (declare (indent 2))
  `(let ((identities ,identities))
     (when identities
       (magit-insert-section (,name)
         (magit-insert-heading ,heading)
         (dolist (identity identities)
           (magit-insert-section ('item identity)
             (insert "  " )
             (twist-package--insert-identity identity)
             (newline)))
         ;; (magit-insert-child-count (magit-current-section))
         (insert ?\n)))))

(defsubst twist-package--insert-yes-or-no (yes)
  (insert (if yes "Yes" "No")))

(defmacro twist-package--propertize-heading (body)
  `(propertize ,body 'face 'magit-section-heading))

(defmacro twist-package--insert-dlist (indent-level &rest rows)
  (declare (indent 1))
  (let* ((width (1+ (cl-loop for header in (mapcar #'car rows)
                             maximize (length header))))
         (indent-string (make-string indent-level ?\ ))
         (th (format "%s%%-%ds" indent-string width)))
    `(progn
       ,@(mapcar (pcase-lambda (`(,header ,visible ,exp))
                   `(when ,visible
                      (magit-insert-section ('row ,header)
                        (insert (twist-package--propertize-heading
                                 (format ,th ,header)))
                        ,exp
                        (newline))))
                 rows))))

(defmacro twist-package--file-list (src files)
  (declare (indent 1))
  `(let ((src ,src))
     (dolist (file ,files)
       (magit-insert-section ('file file)
         (insert "  ")
         (insert-text-button file
                             'type 'twist-package-source-file
                             'help-args (list src file))
         (newline)))))

;;;;; Section functions

(defun twist-package-header-section (data)
  (let ((ename (twist-package-lookup ename data)))
    (insert (propertize (capitalize ename) 'face 'twist-package-title-face)
            "\n"
            (if-let (summary (twist-package-lookup (headers summary) data))
                (concat (propertize summary 'face 'twist-package-summary-face) "\n")
              "")
            ?\n)))

(defun twist-package-documents-section (data)
  (let ((ename (twist-package-lookup ename data)))
    (magit-insert-section ('documents)
      (let* ((src (twist-package-lookup src data))
             (source-files (directory-files src nil
                                            (concat "^" twist-package-community-regexp "$")
                                            'nosort))
             (license-file (seq-find (lambda (file)
                                       (string-match-p (concat "^" twist-package-license-regexp)
                                                       file))
                                     source-files))
             ;; If elx-license-use-licensee is set to t (default) and the user
             ;; doesn't have the executable, it will throw an error. The user
             ;; may not be aware of it, but this function shouldn't fail because
             ;; of it, so wrap it in `ignore-errors'.
             (license-name (ignore-errors
                             (elx-license
                              (expand-file-name (twist-package-lookup mainFile data)
                                                (twist-package-lookup src data))))))
        (if license-file
            (insert-text-button (or license-name "LICENSE")
                                'type 'twist-package-source-file
                                'help-args (list src license-file)
                                'file-kind 'license)
          (insert (or license-name "No detected license")))
        (insert " ")

        (cl-macrolet
            ((insert-source-button
              (label regexp file-kind)
              `(let ((file (seq-find (lambda (file)
                                       (string-match-p ,regexp file))
                                     source-files)))
                 (when file
                   (insert-text-button ,label 'type 'twist-package-source-file
                                       'help-args (list src file)
                                       'file-kind ,file-kind)
                   (insert " ")
                   file))))
          (insert-source-button "README" twist-package-readme-regexp
                                'readme)
          (insert-source-button "CONTRIBUTING" twist-package-contributing-regexp
                                'contributing)))

      (dolist (output (or twist-package-outputs
                          (mapcar (lambda (x) (cons x nil))
                                  (twist-session-package-outputs ename))))
        (unless (equal (car output) "out")
          (pcase-let ((`(,label ,subdir ,find-file-fn)
                       (pcase (car output)
                         ("info" '("Info" "share/info/" info)))))
            (if (cdr output)
                (insert-text-button label
                                    'type 'twist-package-output
                                    'help-args (list (cdr output) subdir find-file-fn))
              (insert label)))))

      (newline)
      (insert ?\n))))

(defun twist-package-metadata-section (data)
  (let ((ename (twist-package-lookup ename data))
        (version (twist-package-lookup version data))
        (last-modified (twist-package-lookup lastModified data))
        (origin (twist-package-lookup origin data))
        (homepage (twist-package-lookup (headers Homepage) data))
        (keywords (twist-package-lookup (headers Keywords) data))
        (src (twist-package-lookup src data)))
    (twist-package--insert-dlist 0
      ("Version:" version
       (insert version))
      ("Type:" t
       (let ((type (twist-package-lookup (inventory type) data))
             (path (twist-package-lookup (inventory path) data))
             (url (twist-package-lookup (inventory url) data)))
         (if (equal type "melpa")
             (insert-text-button type 'type 'twist-package-recipe
                                 'help-args (list path ename))
           (insert type))
         (when url
           (insert " ")
           (insert-text-button url 'type 'twist-package-url
                               'help-args (list url)))))
      ("Output:" (cdr (assoc "out" twist-package-outputs))
       (let* ((dir (expand-file-name "share/emacs/site-lisp/"
                                     (cdr (assoc "out" twist-package-outputs))))
              (files (thread-last (directory-files-recursively dir ".+" nil nil 'symlinks)
                       (mapcar (lambda (path) (string-remove-prefix dir path)))
                       (cl-remove-if (lambda (name)
                                       (string-match-p
                                        (concat "^" twist-package-community-regexp "$")
                                        name))))))
         (when files
           (pcase-dolist (`(,ext . ,files-by-ext)
                          (seq-sort-by
                           #'car #'string<
                           (seq-group-by #'file-name-extension files)))
             (pcase ext
               ('()
                (dolist (file files-by-ext)
                  (insert-text-button file
                                      'type 'twist-package-output
                                      'help-args (list dir file))
                  (insert " "))
                (insert (string-join files-by-ext " ") " "))
               ("elc")
               (_
                (insert (propertize ext 'help-echo
                                    (string-join files-by-ext " "))
                        (format "(%d) " (length files-by-ext)))))))
         (insert-text-button "Browse"
                             'type 'twist-package-output-dir
                             'help-args (list dir))))
      ("Source:" t
       (insert-text-button src 'type 'twist-package-store-path
                           'help-args (list src)))
      ("Last modified:" last-modified
       (insert (concat (format-time-string "%F %R" last-modified)
                       (format " (%d days ago)"
                               (/ (- (float-time) (float-time last-modified))
                                  86400)))))
      ("Origin:" origin
       (insert (twist-flake-alist-to-url origin)))
      ("Home page:" homepage
       (insert-text-button homepage 'type 'twist-package-url
                           'help-args (list homepage)))
      ("Keywords:" keywords
       (dolist (keyword keywords)
         (let ((desc (cdr (assq (intern keyword) finder-known-keywords))))
           (insert (propertize keyword
                               'help-echo (or desc "Unknown keyword")
                               'face (if desc
                                         'twist-package-known-face
                                       'twist-package-unknown-face))
                   " ")))))

    (insert ?\n)))

(defun twist-package-authors-section (data)
  (twist-package--people-section 'authors "Authors"
    (thread-last (twist-package-lookup author data)
      (twist-package--to-list))))

(defun twist-package-maintainers-section (data)
  (twist-package--people-section 'maintainers "Maintainers"
    (thread-last (twist-package-lookup (headers Maintainer) data)
      (twist-package--to-list))))

(defun twist-package-license-section (data)
  (let* ((license (twist-package-lookup (meta license) data))
         (fullName (cdr (assq 'fullName license)))
         (deprecated (cdr (assq 'deprecated license)))
         (spdxId (cdr (assq 'spdxId license)))
         (url (cdr (assq 'url license))))
    (when spdxId
      (magit-insert-section ('license)
        (magit-insert-heading
          (twist-package--propertize-heading "SPDX License"))
        (let ()
          (twist-package--insert-dlist 2
            ("Name: " fullName
             (insert fullName))
            ("SPDX-ID:" spdxId
             (insert spdxId))
            ("Deprecated:" deprecated
             (insert "Yes"))
            ("Free:" license
             (twist-package--insert-yes-or-no (cdr (assq 'free license))))
            ("Redistributable:" license
             (twist-package--insert-yes-or-no (cdr (assq 'free license))))
            ("URL:" url
             (insert-text-button url 'type 'twist-package-url
                                 'help-args (list url)))))
        (insert ?\n)))))

(defun twist-package-lisp-files-section (data)
  (let* ((src (twist-package-lookup src data))
         (main-file (twist-package-lookup mainFile data))
         (other-files (thread-last (cl-remove main-file
                                              (twist-package-lookup lispFiles data)
                                              :test #'equal)
                        (seq-sort #'string<))))
    (magit-insert-section ('lisp-files)
      (magit-insert-heading "Lisp Files")
      (twist-package--file-list src (list main-file))
      (twist-package--file-list src other-files)
      (magit-insert-child-count (magit-current-section))
      (insert ?\n))))

(defun twist-package-other-files-section (data)
  (when-let (other-files (thread-last (seq-difference
                                       (thread-last (twist-package-lookup files data)
                                         (mapcar #'car)
                                         ;; Now the files attribute is an
                                         ;; attrset, so file names are returned
                                         ;; as symbols. There should be a better API.
                                         (mapcar #'symbol-name))
                                       (twist-package-lookup lispFiles data))
                           (seq-sort #'string<)))
    (magit-insert-section ('other-files)
      (magit-insert-heading "Other Files")
      (twist-package--file-list (twist-package-lookup src data)
        other-files)
      (insert ?\n))))

(defun twist-package--insert-package-name (name)
  "Insert a button or text to a package.

NAME must be a string. If the package is installed as a
third-party dependency in the configuration, it inserts a button.
Otherwise, it inserts a text."
  (if (map-contains-key twist-session-packages name)
      (insert-text-button name
                          'type 'twist-package-name
                          'help-args (list name))
    (insert name)))

(defun twist-package-deps-section (data)
  (let ((deps (twist-package-lookup packageRequires data)))
    (when deps
      (magit-insert-section ('dependencies)
        (magit-insert-heading "Dependencies")

        (pcase-dolist (`(,name . ,version)
                       (cons (or (assq 'emacs deps)
                                 ;; Some authors forget to add emacs
                                 '(emacs))
                             (cl-remove 'emacs deps :key #'car)))
          (magit-insert-section ('dep name)
            (insert "  ")
            (twist-package--insert-package-name (symbol-name name))
            (let ((pad (- 14 (length (symbol-name name)))))
              (when (> pad 0)
                (insert (make-string pad ?\ ))))
            (when version
              (insert (format " >= %s" version)))
            (newline)))
        
        ;; (magit-insert-child-count (magit-current-section))
        (insert ?\n)))))

(defun twist-package-reverse-deps-section (data)
  (let* ((ename (twist-package-lookup ename data))
         (reverse-deps (twist-session-reverse-deps ename)))
    (when reverse-deps
      (magit-insert-section ('reverse-deps)
        (magit-insert-heading "Reverse Dependencies")

        (dolist (rev-dep reverse-deps)
          (magit-insert-section ('rev-dep rev-dep)
            (insert "  ")
            (twist-package--insert-package-name rev-dep)
            (newline)))

        ;; (magit-insert-child-count (magit-current-section))
        (insert ?\n)))))

;;;; Commands available in package buffers

(defun twist-package-find-license-file (data)
  (if-let (files (directory-files
                  (file-name-as-directory
                   (if (stringp data)
                       data
                     (twist-package-lookup lispFiles data)))
                  t twist-package-license-regexp))
      (find-file-read-only (car files))
    (user-error "No license file is found in the directory")))

(defun twist-package-find-file (dir file)
  (find-file-read-only (expand-file-name file dir)))

(defun twist-package-discover-author (author-or-email)
  "Discover packages by AUTHOR-OR-EMAIL."
  (if (and (require 'epkg-list nil t)
           (fboundp 'epkg-list-packages-by-author))
      (epkg-list-packages-by-author author-or-email)
    (user-error "Package epkg is required to use this feature")))

(defun twist-package-build (ename dest-buffer)
  (twist-session-build-package ename
    (lambda (result)
      (with-current-buffer dest-buffer
        (setq twist-package-build-status 'success
              twist-package-outputs result)
        (revert-buffer)))))

(defun twist-package-browse-output-dir (store &optional dir _fn)
  (dired (if dir
             (expand-file-name dir store)
           store)))

(defun twist-package-find-file-in-output (store dir &optional fn)
  (let* ((path (expand-file-name dir store))
         (files (directory-files path nil "^[^.]")))
    (funcall (or fn #'find-file-read-only)
             (expand-file-name (if (> (length files) 1)
                                   (completing-read "Select a file: "
                                                    (sort files
                                                          (lambda (a b)
                                                            (< (length a) (length b)))))
                                 (car files))
                               path))))

;;;; Extra features

(defun twist-package-bookmark-record ()
  (twist-bookmark-make-record nil 'package twist-package-ename))

(provide 'twist-package)
;;; twist-package.el ends here
