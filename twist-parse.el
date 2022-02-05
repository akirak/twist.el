;;; twist-parse.el --- Parsing utilities -*- lexical-binding: t -*-

(defconst twist-parse-store-regexp-1
  (rx "/nix/store/"
      ;; hash
      (+ (any alnum))
      "-"
      ;; name
      (group (group (+ (any "-" alnum)))
             (?  "-"
                 (+ digit)
                 (* (and "." (+ digit)))
                 (?  "-" (+ (any alpha)))))
      ".drv"))

(defconst twist-parse-error-regexp-1
  (rx bol (* space)
      "error: builder for '"
      (group (regexp twist-parse-store-regexp-1))
      "' failed"))

(defun twist-parse-search-store-from-error ()
  (save-match-data
    (when (re-search-forward twist-parse-error-regexp-1 nil t)
      (match-string 1))))

(defun twist-parse-pname-from-store (store-path)
  (save-match-data
    (when (string-match twist-parse-store-regexp-1 store-path)
      (match-string 2 store-path))))

(provide 'twist-parse)
;;; twist-parse.el ends here
