; Copyright 2011 Google Inc. All Rights Reserved.
; This file is available under the Apache license.

(defvar emtail-mode-hook nil)

(defcustom emtail-indent-offset 2
  "Indent offset for `emtail-mode'.")

(defvar emtail-mode-syntax-table
  (let ((st (make-syntax-table)))
    ; Add _ to :word: class
    (modify-syntax-entry ?_ "." st)

    ; Comments
    (modify-syntax-entry ?\# "< b" st)
    (modify-syntax-entry ?\n "> b" st)

    ; Regex
    (modify-syntax-entry ?/ "|" st)

    ; String
    (modify-syntax-entry ?\" "\"" st)

    ; Square brackets like parens
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)

    ; Operators
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?! "." st)
    st)
  "Syntax table used while in `emtail-mode'.")

(defconst emtail-mode-types
  '("counter" "gauge")
  "All types in the emtail language.  Used for font locking.")

(defconst emtail-mode-keywords
  '("as" "by" "const" "hidden" "def" "next")
  "All keywords in the emtail language.  Used for font locking.")

(defconst emtail-mode-builtins
  '("len" "strptime" "timestamp")
  "All builtins in the emtail language.  Used for font locking.")

(defvar emtail-mode-font-lock-defaults
  (eval-when-compile
    (list
     (cons (concat "\\<"
                   (regexp-opt emtail-mode-types 'words)
                   "\\>")
           'font-lock-type-face)
     (cons (concat "\\<"
                   (regexp-opt emtail-mode-builtins 'words)
                   "\\>")
           'font-lock-builtin-face)
     (cons (concat "\\<"
                   (regexp-opt emtail-mode-keywords 'words)
                   "\\>")
           'font-lock-keyword-face)
     (cons "\\<@[a-zA-Z0-9_]+\\>" 'font-lock-function-name-face)
     (cons "\\<\\$?[a-zA-Z0-9_]+\\>"  'font-lock-variable-name-face)
     )))

;;;###autoload
(define-derived-mode emtail-mode awk-mode "emtail"
  "Major mode for editing emtail programs."
  :syntax-table emtail-mode-syntax-table
  (set (make-local-variable 'paragraph-separate) "^[ \t]*$")
  (set (make-local-variable 'paragraph-start) "^[ \t]*$")
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  ;; Font lock
  (set (make-local-variable 'font-lock-defaults)
       '(emtail-mode-font-lock-defaults))
  (setq indent-tabs-mode nil))

(defun emtail-mode-reload ()
  "Reload emtail-mode.el and put the current buffer into emtail-mode.  Useful for debugging."
  (interactive)
  (unload-feature 'emtail-mode)
  (add-to-list 'load-path "/home/jaq/src/emtail")
  (require 'emtail-mode)
  (emtail-mode))

(provide 'emtail-mode)

(add-to-list 'auto-mode-alist (cons "\\.em$" #'emtail-mode))
