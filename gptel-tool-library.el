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

(defun gptel-tool-library--debug-log (log)
  "Print LOG to `gptel-tool-library-debug-buffer' if debug logging is enabled."
  (let ((buffer (get-buffer-create gptel-tool-library-debug-buffer)))
    (with-current-buffer gptel-tool-library-debug-buffer
      (insert (format "%s\n" log)))))

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

(defun gptel-tool-library-load-module (module-name)
  "Load library `gptel-tool-library-MODULE-NAME' and add its tools to `gptel-tools'."
  (let* ((module-sym (intern (format "gptel-tool-library-%s" module-name))))
    (condition-case err
        (progn
          (require module-sym)
          (dolist (n '("" "-unsafe" "-maybe-safe"))
            (let ((cond-var-sym (intern (format "gptel-tool-library-use%s" n)))
                  (tool-var-sym (intern (format "gptel-tool-library-%s-tools%s" module-name n))))
              (when (and (boundp cond-var-sym) (symbol-value cond-var-sym))
                (when (boundp tool-var-sym)
                  (let ((module-tools (symbol-value tool-var-sym)))
                    (setq gptel-tools
                          (cl-remove-if (lambda (tool)
                                          (member tool module-tools))
                                        gptel-tools))
                    (setq gptel-tools (append gptel-tools module-tools))))))))

          (error (message "Error loading module %s: %s" module-name err)))))

(defun gptel-tool-library-unload-module (module-name)
  "Remove tools from `gptel-tools' that belong to module MODULE-NAME."
  (dolist (n '("" "-unsafe" "-maybe-safe"))
    (let ((tool-var-sym (intern (format "gptel-tool-library-%s-tools%s" module-name n))))
      (when (boundp tool-var-sym)
        (setq gptel-tools
              (cl-remove-if (lambda (tool)
                              (member tool (symbol-value tool-var-sym)))
                            gptel-tools))))))

(provide 'gptel-tool-library)

;;; gptel-tool-library.el ends here
