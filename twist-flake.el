;;; twist-flake.el --- Utility functions related to Nix flakes -*- lexical-binding: t -*-

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

;; This library is a collection of utility function for Twist.

;;; Code:

(require 'subr-x)

(defun twist-flake-normalize-path (directory)
  "Normalize a path to DIRECTORY."
  (string-remove-suffix "/" (expand-file-name directory)))

(defun twist-flake-alist-to-url (origin)
  "Convert ORIGIN into a plain URL format."
  (let-alist origin
    (concat (pcase \.type
              ("github" (format "github:%s/%s" \.owner \.repo))
              ("git" (concat "git+" \.url))
              ("tarball" \.url)
              (_ (format "error: %s: %s" \.type origin)))
            (if \.ref
                (format "?ref=%s" \.ref)
              ""))))

(provide 'twist-flake)
;;; twist-flake.el ends here
