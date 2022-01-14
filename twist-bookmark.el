;;; twist-bookmark.el --- Bookmark support -*- lexical-binding: t -*-

(require 'bookmark)

;;;###autoload
(defun twist-bookmark-handler (bookmark)
  (let-alist (bookmark-get-bookmark-record bookmark)
    ;; Unless the current session has already loaded exactly the same package,
    ;; reopen a session.
    (let ((active (twist-session-process-live-p)))
      (unless (and active
                   (equal (expand-file-name \.flake)
                          (twist-session-flake))
                   (equal \.package (twist-session-config-package)))
        (when active
          (twist-session-close))
        (twist-session-open \.flake \.package)))

    (pcase \.type
      (`packages
       ;; FIXME: Somehow `switch-to-buffer' is necessary, even if it should be
       ;; called inside `twist-packages'.
       (switch-to-buffer (twist-packages)))
      (`package
       (twist-package (car \.args)))
      (_
       (error "Unsupported twist bookmark type: %s" \.type)))))

(defun twist-bookmark-make-record (name type &rest args)
  (let ((flake (abbreviate-file-name (twist-session-flake)))
        (package (twist-session-config-package)))
    `(,(concat flake "#" package
               (if args
                   (format " %s" args)
                 ""))
      (handler . twist-bookmark-handler)
      (flake . ,flake)
      (package . ,package)
      (type . ,type)
      (args . ,args))))

(provide 'twist-bookmark)
;;; twist-bookmark.el ends here
