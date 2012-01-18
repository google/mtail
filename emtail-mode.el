; Copyright 2011 Google Inc. All Rights Reserved.
; This file is available under the Apache license.

(defcustom emtail-indent-offset 2
  "Indent offset for `emtail-mode'.")

(defvar emtail-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?/ "|" table)
    table)
  "Syntax table used while in `emtail-mode'.")

(defvar emtail-font-lock-defaults
      `(("\\<\\(FIXME\\|TODO\\)" 1 font-lock-warning-face prepend)
        ("\\<\\(label\\|inc\\|push\\|pop\\|strptime\\)\\>" .
         font-lock-keyword-face)
        ("\\<\\$[a-zA-Z0-9]+\\>" . font-lock-variable-name-face)))

(define-derived-mode emtail-mode nil "emtail"
  "Major mode for editing emtail programs."
  (setq font-lock-defaults emtail-font-lock-defaults)
  (modify-syntax-entry ?# "< b" emtail-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" emtail-mode-syntax-table)
  (modify-syntax-entry ?/ "|" emtail-mode-syntax-table)
  (setq paragraph-separate "^[ \t]*$")
  (setq paragraph-start "^[ \t]*$")
  (setq comment-start "# ")
  (setq comment-end "")
  (setq comment-start-skip "# *"))


(provide 'emtail-mode)
