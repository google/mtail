; Copyright 2011 Google Inc. All Rights Reserved.
; This file is available under the Apache license.

(defcustom mtail-indent-offset 2
  "Indent offset for `mtail-mode'.")

(defvar mtail-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?/ "|" table)
    table)
  "Syntax table used while in `mtail-mode'.")

(defvar mtail-font-lock-defaults
      `(("\\<\\(FIXME\\|TODO\\)" 1 font-lock-warning-face prepend)
        ("\\<\\(label\\|inc\\|push\\|pop\\|strptime\\)\\>" .
         font-lock-keyword-face)
        ("\\<\\$[a-zA-Z0-9]+\\>" . font-lock-variable-name-face)))

(define-derived-mode mtail-mode nil "mtail"
  "Major mode for editing mtail programs."
  ;(setq font-lock-defaults mtail-font-lock-defaults)
  ;; (modify-syntax-entry ?# "< b" mtail-mode-syntax-table)
  ;; (modify-syntax-entry ?\n "> b" mtail-mode-syntax-table)
  ;; (modify-syntax-entry ?/ "|" mtail-mode-syntax-table)
  (setq paragraph-separate "^[ \t]*$")
  (setq paragraph-start "^[ \t]*$")
  (setq comment-start "# ")
  (setq comment-end "")
  (setq comment-start-skip "# *"))


(provide 'mtail-mode)
