;;; gptel-tool-library-elisp.el --- Functions to help with elisp development -*- lexical-binding: t; -*-
;;
;; Author: Bernd Wachter
;;
;; Copyright (c) 2025 Bernd Wachter
;;
;; Keywords: tools
;;
;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details. http://www.gnu.org/copyleft/gpl.html
;;
;;; Commentary:
;;
;; This provides LLM access to some information for efficient automated elisp
;; editing. This is building on the buffer module - and generally should be
;; doable with just the buffer module, but especially LLMs with a small context
;; window might prefer having specialised tools operating on smaller context.
;;
;;; Code:

(require 'gptel-tool-library)

(defvar gptel-tool-library-elisp-tools '()
  "The list of elisp related tools")

(defvar gptel-tool-library-elisp-tools-maybe-safe '()
  "The list of elisp related tools which may be destructive, but typically
the LLM behaves.")

(defvar gptel-tool-library-elisp-tools-unsafe '()
  "The list of buffer related tools which are not safe.")

(defun gptel-tool-library-elisp--run-eval (command)
  "Return output of elisp eval COMMAND."
  (gptel-tool-library--debug-log (format "eval %s" command))
  (eval (if (stringp command)
            (car (read-from-string (format "(progn %s)" command)))
          command)))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-elisp-tools-unsafe
 :function #'gptel-tool-library-elisp--run-eval
 :name  "eval"
 :description "Use eval to evaluate elisp code. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "command"
                     :type string
                     :description "The elisp code to evaluate."))
 :category "elisp"
 :confirm t)

(defun gptel-tool-library-elisp--defun-region (function-name &optional buffer)
  "Return (START . END) points around FUNCTION-NAME defun in BUFFER.

If BUFFER is nil, use the current buffer. Points are just before and after
the defun."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp (concat "^(\\s-*defun\\s-+" (regexp-quote function-name) "\\_>") nil t)
          (let (start end)
            (beginning-of-defun)
            (setq start (point))
            (end-of-defun)
            (setq end (point))
            (cons start end))
        (message "Function `%s` not found in buffer %s" function-name (buffer-name))
        nil))))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-elisp-tools
 :function #'gptel-tool-library-elisp--defun-region
 :name  "defun-region"
 :description "Read starting end end point of a function definition."
 :args (list '(:name "function"
                     :type string
                     :description "The name of the function to search.")
             '(:name "buffer"
                     :type string
                     :description "The buffer to search in"))
 :category "elisp")

(defun gptel-tool-library-elisp--replace-defun-region (function-name new-string &optional buffer)
  "Replace the entire FUNCTION-NAME region with NEW-STRING.

Use BUFFER or current buffer if BUFFER is nil."
  (let ((bounds (gptel-tool-library-elisp--defun-region function-name buffer)))
    (if bounds
        (with-current-buffer (or buffer (current-buffer))
          (save-excursion
            (goto-char (car bounds))
            (delete-region (car bounds) (cdr bounds))
            (insert new-string)))
      (message "Function `%s` not found for replacement" function-name))))

(defun gptel-tool-library-elisp--smerge-replace-defun-region (function-name new-string &optional buffer)
  "Insert smerge conflict markers for FUNCTION-NAME region with NEW-STRING.

Operate in BUFFER or current buffer if BUFFER is nil."
  (interactive "sFunction name: \nsNew function text: \nBBuffer (optional): ")
  (let ((buf (or buffer (current-buffer)))
        bounds old-text conflict-text)
    (setq bounds (gptel-tool-library-elisp--defun-region function-name buf))
    (if (not bounds)
        (message "Function `%s` not found in buffer %s" function-name (buffer-name buf))
      (with-current-buffer buf
        (setq old-text (buffer-substring-no-properties (car bounds) (cdr bounds)))
        (setq conflict-text
              (concat "<<<<<<< FUNCTION BEFORE REPLACE\n"
                      old-text
                      "=======\n"
                      new-string
                      "\n>>>>>>> FUNCTION AFTER REPLACE\n"))
        (goto-char (car bounds))
        (delete-region (car bounds) (cdr bounds))
        (insert conflict-text)
        (smerge-mode 1)
        (message "Inserted smerge-style conflict for function `%s`" function-name)))))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-elisp-tools-maybe-safe
 :function #'gptel-tool-library-elisp--smerge-replace-defun-region
 :name  "smerge-replace-defun-region"
 :description "Search for a function name, and replace the complete defun with a diff block. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "function-name"
                     :type string
                     :description "The elisp code to evaluate.")
             '(:name "new-string"
                     :type string
                     :description "The new function definition")
             '(:name "buffer"
                     :type string
                     :description "The buffer to perform the replacement"))
 :category "elisp")

(defun gptel-tool-library-elisp-variable-doc (name)
  "Try to return documentation for variable NAME"
  (gptel-tool-library--debug-log (format "elisp-variable-doc: %s" name))
  (let ((var (intern-soft name)))
    (if (and var (boundp var))
        (documentation-property var 'variable-documentation)
      (format "No documentation found for variable: %s" name))))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-elisp-tools
 :function #'gptel-tool-library-elisp-variable-doc
 :name "elisp-variable-doc"
 :category "elisp"
 :description "Retrieve the documentation for an Elisp variable."
 :args (list '(:name "name"
                     :type "string"
                     :description "The name of the variable to retrieve documentation for.")))

(defun gptel-tool-library-elisp-function-doc (name)
  "Try to return documentation for function NAME"
    (gptel-tool-library--debug-log (format "elisp-function-doc: %s" name))
    (let ((func (intern-soft name)))
      (if (and func (fboundp func))
          (documentation func)
        (format "No documentation found for function: %s" name))))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-elisp-tools
 :function #'gptel-tool-library-elisp-function-doc
 :name "elisp-function-doc"
 :category "elisp"
 :description "Retrieve the documentation for an Elisp function."
 :args (list '(:name "name"
                     :type "string"
                     :description "The name of the function to retrieve documentation for.")))

(defun gptel-tool-library-elisp-describe-symbol (name)
  "Return the source code for function or variable NAME as a string.
If source isn't found, falls back to the Emacs Lisp object sexp."
       (let* ((sym (if (symbolp name) name (intern name)))
              (callable (or (functionp sym) (macrop sym)))
              (find-fn (if callable #'find-function-noselect #'find-variable-noselect)))
         (condition-case nil
             (let* ((res (funcall find-fn sym))
                    (buf (car res))
                    (pos (cdr res)))
               (with-current-buffer buf
                 (save-excursion
                   (goto-char pos)
                   (buffer-substring-no-properties
                    (point)
                    (progn (end-of-defun) (point))))))
           (error
            (let ((obj (if callable
                           (symbol-function sym)
                         (symbol-value sym))))
              (pp-to-string obj))))))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-elisp-tools
 :function #'gptel-tool-library-elisp-describe-symbol
 :name "elisp-describe-symbol"
 :category "elisp"
 :description "Return source code for function or variable."
 :args (list '(:name "name"
                     :type "string"
                     :description "The name of the symbol to receive sources for.")))

(defun gptel-tool-library-elisp-describe-symbol-fuzzy (_name &optional _limit)
  "List functions and variables fuzzily matching NAME, with a summary.

Optionally limit the number of results returned."
  (lambda (name &optional limit)
    (gptel-tool-library--debug-log (format "elisp_fuzzy_match: %s, limit: %s" name limit))
    (let ((matches '()))
      ;; Search functions
      (mapatoms (lambda (sym)
                  (when (and (fboundp sym) (string-match-p name (symbol-name sym)))
                    (push (format "Function: %s - %s"
                                  (symbol-name sym)
                                  (or (ignore-errors
                                        (documentation sym))
                                      "No documentation available."))
                          matches))))
      ;; Search variables
      (mapatoms (lambda (sym)
                  (when (and (boundp sym) (string-match-p name (symbol-name sym)))
                    (push (format "Variable: %s - %s"
                                  (symbol-name sym)
                                  (or (ignore-errors
                                        (documentation-property sym 'variable-documentation))
                                      "No documentation available."))
                          matches))))
      ;; Apply limit if specified
      (when (and limit (> (length matches) limit))
        (setq matches (seq-take matches limit)))
      (if matches
          (string-join matches "
")
        (format "No matches found for name: %s" name)))))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-elisp-tools
 :function #'gptel-tool-library-elisp-describe-symbol-fuzzy
 :name "elisp-fuzzy-match"
 :category "elisp"
 :description "List all functions and variables that fuzzily match the given name, along with a one-sentence summary, with an optional limit on the number of results."
 :args (list '(:name "name"
                     :type "string"
                     :description "The name to match against available functions and variables.")
             '(:name "limit"
                     :type "integer"
                     :optional t
                     :description "The maximum number of results to return. If nil, returns all matches.")))

(provide 'gptel-tool-library-elisp)
;;; gptel-tool-library-elisp.el ends here
