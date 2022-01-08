;;; twist-packages.el --- Tablist interface to packages in Twist -*- lexical-binding: t -*-

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

;; This library provides a tablist interface which displays packages in a Twist
;; configuration.

;;; Code:

(require 'twist-session)

(require 'tabulated-list)
(require 'map)

;; TODO: Use tablist once it becomes active again.
;;
;; I experience a bug inside call of `tablist-filter-eval'. It would be a bad
;; idea to work around the tablist package for twist, so I won't use the package
;; now.

;; (require 'tablist)

(defconst twist-packages-buffer "*Twist Packages*")

(defcustom twist-packages-column-alist
  '((ename
     ename
     "Name" 20 t)
    (version
     version
     "Version" 8 nil
     :right-align t)
    (owner
     (origin owner)
     "Owner" 13 t)
    (license
     (meta license shortName)
     "License" 9 t)
    (keywords
     (headers Keywords (lambda (xs) (string-join xs ",")))
     "Keywords" 20 nil)
    (lastModified
     (lastModified (lambda (unix) (when unix (format-time-string "%F" unix))))
     "Last Modified" 12 t
     :right-align t :pad-right 2)
    (author
     (author (lambda (xs) (cl-etypecase xs
                            (list (car xs))
                            (string xs))))
     "Author" 27 t)
    (inventoryType
     (inventory type)
     "Type" 8 t)
    (summary
     (meta description)
     "Summary" 25 nil))
  "List of columns of the table in the package list buffer.

The fifth element in each column can be either a symbol or a
list of symbols, optionally followed by a formatter function. The
list of symbols is a path that traverses the tree of package
data, and the formatter is applied to the result.

For other settings, see `tabulated-list-format'."
  :type '(repeat (list (symbol :tag "Column ID")
                       (choice :tag "Field(s) and optional formatter"
                               symbol
                               (repeat (choice symbol function)))
                       (string :tag "Header")
                       (integer :tag "Width")
                       (choice :tag "Sort"
                               (const :tag "Sortable by string comparison" t)
                               (const :tag "Cannot be used for sorting" nil)
                               (function :tag "Provide a sorting predicate"))
                       (plist :tag "Additional column properties"
                              :inline t))))

(defcustom twist-packages-default-columns
  '(ename version lastModified inventoryType license owner author keywords summary)
  "The default list of columns shown in the package list.

For the values, see `twist-packages-column-alist'."
  :type '(repeat (symbol :tag "Column ID")))

;; Local variables
(defvar-local twist-packages-row-fn nil)

(defvar twist-packages-mode-map
  (let ((m (make-composed-keymap nil tabulated-list-mode-map)))
    (define-key m (kbd "C-m") #'twist-packages-show-package)
    m))

(define-derived-mode twist-packages-mode tabulated-list-mode
  "Twist Packages"
  ;; TODO: Set imenu functions
  ;; (tablist-minor-mode)
  (add-hook 'tabulated-list-revert-hook 'twist-packages-refresh nil t))

;;;###autoload
(defun twist-packages (&optional refresh)
  "Browse packages in a tabulated interface.

It works on `twist-comint-buffer' which should be created by
  `twist-session-open'."
  (interactive "P")
  (twist-session-ensure
    (let ((default-directory (twist-session-directory)))
      (with-current-buffer (get-buffer-create twist-packages-buffer)
        (twist-packages-mode)
        (twist-packages-set-columns twist-packages-default-columns)
        (setq tabulated-list-entries
              (lambda ()
                (let ((hash (twist-session-packages)))
                  (prog1 (map-apply (lambda (ename data)
                                      (list ename (funcall twist-packages-row-fn data)))
                                    hash)
                    (message "%s packages in the configuration"
                             (map-length hash))))))
        (twist-packages-refresh)
        (switch-to-buffer (current-buffer))))))

(defun twist-packages-set-columns (column-ids)
  "Set the columns to COLUMN-IDS."
  (let ((columns (mapcar (lambda (symbol)
                           (or (cdr (assq symbol twist-packages-column-alist))
                               (error "Column %s does not exist in twist-packages-column-alist"
                                      symbol)))
                         column-ids)))
    (setq tabulated-list-format (vconcat (mapcar #'cdr columns)))
    (setq twist-packages-row-fn
          (byte-compile
           `(lambda (data)
              (vector ,@(thread-last (mapcar #'car columns)
                          (mapcar (lambda (path)
                                    (let ((path (if (listp path)
                                                    path
                                                  (list path))))
                                      `(or ,(cl-reduce (lambda (acc f)
                                                         (if (symbolp f)
                                                             `(cdr (assq ',f ,acc))
                                                           `(funcall ,f ,acc)))
                                                       (cdr path)
                                                       :initial-value
                                                       `(cdr (assq ',(car path) data)))
                                           "")))))))))))

(defun twist-packages-refresh ()
  "Refresh contents of the package list buffer."
  (tabulated-list-init-header)
  (tabulated-list-print t))

;;;; Commands

(defun twist-packages-show-package (package)
  "Describe PACKAGE at point."
  (interactive (list (tabulated-list-get-id)))
  (pop-to-buffer (twist-package-buffer package)))

(provide 'twist-packages)
;;; twist-packages.el ends here
