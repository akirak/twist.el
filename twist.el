;;; twist.el --- Frontend to Twist configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (nix-mode "0") (elx "0"))
;; Keywords: maint
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

;; Twist.el is a frontend for Twist.

;;; Code:

(require 'twist-session)
(require 'twist-flake)

(defgroup twist nil
  "An alternative machinery for configuration."
  :group 'maint)

(defcustom twist-configuration-package nil
  "Nix package for the Twist configuration."
  :group 'twist
  :type '(choice (const nil)
                 (list (directory :tag "Flake directory")
                       (string :tag "Nix package name"))))

;;;###autoload
(defun twist-connect (&optional directory package)
  "Start a new session inspecting a configuration.

DIRECTORY must be a path to a directory that contains flake.nix,
and PACKAGE must be the name of a package in the flake.

In interactive use, the directory and package defaults to
`twist-configuration-package'. If a prefix argument is given or
the variable is nil, the user will choose a flake interactively."
  (interactive (unless (equal current-prefix-arg '(16))
                 (or (and (not current-prefix-arg)
                          twist-configuration-package)
                     (list (read-directory-name "Flake: ")))))
  (if (equal current-prefix-arg '(16))
      (twist-session-reopen)
    (twist-session-open directory package)))

(defun twist-disconnect ()
  (interactive)
  (twist-session-close))

(defun twist-reconnect ()
  (interactive)
  (twist-session-reopen))

(defun twist-reload ()
  "Reload the current session."
  (interactive)
  (twist-session-reload))

(defalias 'twist-describe-package #'twist-package)
(defalias 'twist-list-packages #'twist-packages)

(provide 'twist)
;;; twist.el ends here
