;;; gptel-tool-library.el --- A collection of tools for gptel
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
;; This implements various tools for LLMS, and implements methods for easy
;; loading and unloading
;;
;;; Code:

(require 'cl-lib)

(defgroup gptel-tool-library nil
  "gptel-tool-library settings"
  :group 'gptel-tool-library)

(defcustom gptel-tool-library-use t
  "Use tools which are safe to use"
  :group 'gptel-tool-library
  :type 'sexp)

(defcustom gptel-tool-library-use-maybe-safe nil
  "Use tools which are most likely safe to use"
  :group 'gptel-tool-library
  :type 'sexp)

(defcustom gptel-tool-library-use-unsafe nil
  "Use tools which are unsafe"
  :group 'gptel-tool-library
  :type 'sexp)

(defcustom gptel-tool-library-max-result-size 40
  "The maximum length of a result in characters.

Depending no the function exceeding that should either throw an error, or filter the result"
  :group 'gptel-tool-library
  :type 'integer)

(defcustom gptel-tool-library-gptel-tools-var 'gptel-tools
  "Symbol of the variable holding the list of GPTel tools."
  :group 'gptel-tool-library
  :type 'symbol)

(defcustom gptel-tool-library-llm-tools-var nil
  "Symbol of the variable holding the list of LLM tools."
  :group 'gptel-tool-library
  :type 'symbol)

(defcustom gptel-tool-library-debug t
  "Log messages to `gptel-tool-library-debug-buffer' when non-nil`"
  :group 'gptel-tool-library
  :type 'symbol)

(defcustom gptel-tool-library-debug-buffer "*gptel-tool-debug*"
  "Buffer for debug output, if debug logging is enabled via `gptel-tool-library-debug'"
  :group 'gptel-tool-library
  :type 'string)

(defconst gptel-tool-library-dir (file-name-directory (or load-file-name
                                                          buffer-file-name)))

(defun gptel-tool-library-append-tools (var tools)
  "Append TOOLS to the list stored in variable VAR non-destructively."
  (set var (append (symbol-value var) tools)))

(defun gptel-tool-library--debug-log (log)
  "Print LOG to `gptel-tool-library-debug-buffer' if debug logging is enabled."
  (let ((buffer (get-buffer-create gptel-tool-library-debug-buffer)))
    (with-current-buffer gptel-tool-library-debug-buffer
      (insert (format "%s\n" log)))))

(defun gptel-tool-library--limit-result (result)
  (if (>= (length (format "%s" result)) gptel-tool-library-max-result-size)
      (format "Results over %s character. Stop. Analyze. Find a different solution, or use a more specific query." gptel-tool-library-max-result-size)
    result))

(defun gptel-tool-library-make-tools (&rest args)
  "Create tools for any known LLMs"
  (let ((tool-list '()))
    (when (fboundp 'gptel-make-tool)
      (add-to-list 'tool-list (apply #'gptel-make-tool args)))
    (when (fboundp 'llm-make-tool)
      (add-to-list 'tool-list (apply #'llm-make-tool args)))))

(defun gptel-tool-library-make-tools-and-register (list &rest args)
  "Create tools for any known LLMS and add them to LIST"
  (gptel-tool-library-append-tools list
   (apply #'gptel-tool-library-make-tools args)))

(defun gptel-tool-library-tool--accessor (tool slot)
  "Return SLOT value from TOOL if TOOl is a known type and accessor exists, else nil.

This helps us in providing a generic wrapper for the tool interfaces of the various
LLM libraries"
  (cond
   ((and (cl-typep tool 'gptel-tool)
         (fboundp (intern (format "gptel-tool-%s" slot))))
    (funcall (intern (format "gptel-tool-%s" slot)) tool))
   ((and (cl-typep tool 'llm-tool)
         (fboundp (intern (format "llm-tool-%s" slot))))
    (funcall (intern (format "llm-tool-%s" slot)) tool))
   (t nil)))

;; the following there are probably the ones we care about most for searching
;; modified tools for removal
(defun gptel-tool-library-tool-function (tool)
  "Convenience binding to get the `function' slot out of a tool"
  (gptel-tool-library-tool--accessor tool "function"))

(defun gptel-tool-library-tool-name (tool)
  "Convenience binding to get the `name' slot out of a tool"
  (gptel-tool-library-tool--accessor tool "name"))

(defun gptel-tool-library-tool-category (tool)
  "Convenience binding to get the `category' slot out of a tool"
  (gptel-tool-library-tool--accessor tool "category"))

(cl-defun gptel-tool-library--remove-tool-by-keys (tools-list &key function name category)
  "Remove tools from TOOLS-LIST (a symbol) matching keys FUNCTION, NAME, CATEGORY.
Ignores tools for which required accessors are not available."
  (setf (symbol-value tools-list)
        (seq-remove
         (lambda (tool)
           (let ((tool-fn (gptel-tool-library-tool-function tool))
                 (tool-name (gptel-tool-library-tool-name tool))
                 (tool-cat (gptel-tool-library-tool-category tool)))
             (and tool-fn tool-name  ;; skip if can't get necessary info
                  (and (or (not function) (eq tool-fn function))
                       (or (not name) (string= tool-name name))
                       (or (not category) (string= tool-cat category))))))
         (symbol-value tools-list))))

(defun gptel-tool-library--remove-tool-by-keys-everywhere (&rest args)
  "Remove the tool from all known tool lists"
  (when (and (boundp gptel-tool-library-gptel-tools-var)
             gptel-tool-library-gptel-tools-var)
    (apply #'gptel-tool-library--remove-tool-by-keys
           (cons (symbol-value 'gptel-tool-library-gptel-tools-var) args)))
  (when (and (boundp gptel-tool-library-llm-tools-var)
             gptel-tool-library-llm-tools-var)
    (apply #'gptel-tool-library--remove-tool-by-keys
           (cons (symbol-value 'gptel-tool-library-llm-tools-var) args))))

(defun gptel-tool-library--register-tool (tool)
  "Register the tool to the type appropriate list, or skip."
  (let* ((tool-type (pcase (type-of tool)
                      ('gptel-tool "gptel")
                      ('llm-tool "llm")
                      (_ (error "Unknown tool type encountered: %s" (type-of tool)))))
         (container-var-sym (intern (format "gptel-tool-library-%s-tools-var" tool-type))))
    (when (and (boundp container-var-sym)
             (symbol-value container-var-sym))
        (let ((target-var-sym (symbol-value container-var-sym)))
          (unless (listp (symbol-value target-var-sym))
            (set target-var-sym '()))
          (set target-var-sym (cons tool (symbol-value target-var-sym)))))))

(defun gptel-tool-library-load-module (module-name)
  "Load library `gptel-tool-library-MODULE-NAME' and add its tools to `gptel-tools'.

Note that this unloads all tools from a module before loading - so for a reload it is
sufficient to just call this function, explicit unloading is not required."
  (let* ((module-sym (intern (format "gptel-tool-library-%s" module-name))))
    (condition-case err
        (progn
          (gptel-tool-library-unload-module module-name)
          (require module-sym)
          (dolist (n '("" "-unsafe" "-maybe-safe"))
            (let* ((cond-var-sym (intern (format "gptel-tool-library-use%s" n)))
                   (tool-var-sym (intern (format "gptel-tool-library-%s-tools%s" module-name n)))
                   (module-category-sym (intern (format "gptel-tool-library-%s-category-name" module-name)))
                   (category (if (boundp module-category-sym)
                                 (symbol-value module-category-sym)
                               module-name)))
              (when (and (boundp cond-var-sym) (symbol-value cond-var-sym))
                (when (boundp tool-var-sym)
                  (dolist (tool (symbol-value tool-var-sym))
                    ;; refuse loading if categories are incorrectly defined
                    (when (eq (type-of tool) 'gptel-tool)
                      (let ((tool-category (gptel-tool-library-tool-category tool)))
                        (gptel-tool-library--debug-log
                         (format "Processing tool load for %s..."
                                 (gptel-tool-library-tool-name tool)))
                        (unless
                            (string=
                             (if (symbolp tool-category) (symbol-name tool-category) tool-category)
                             (if (symbolp category) (symbol-name category) category))
                          (error
                           (message
                            (format "Category %s does not match tool category %s"
                                    category tool-category))))))
                    (gptel-tool-library--register-tool tool)))))))
      (error (message "Error loading module %s: %s" module-name err)))))

(defun gptel-tool-library-unload-module (module-name)
  "Remove tools from `gptel-tools' that belong to module MODULE-NAME.

This expects that all tools from the module have the same category, and the category
either matches the module name, or a `category-name' variable is set in the module
namespace."
  (let* ((module-category-sym (intern (format "gptel-tool-library-%s-category-name" module-name)))
         (category (if (boundp module-category-sym)
                       (symbol-value module-category-sym)
                     module-name)))
    ;; TODO: for llm we should loop through existing variables here to remove tools
    (let* ((feature-name (format "gptel-tool-library-%s" module-name))
           (feature-sym (intern feature-name)))
      (when (featurep feature-sym)
        (unload-feature feature-sym t)))
    ;; nuke all existing tools, by category - we don't know
    ;; if tool definitions have changed, so might no longer match
    ;; llm tools don't have categories, so we'll still have to try
    ;; to remove them by name individually later on
    (gptel-tool-library--remove-tool-by-keys-everywhere :category category)))

(provide 'gptel-tool-library)

;;; gptel-tool-library.el ends here
