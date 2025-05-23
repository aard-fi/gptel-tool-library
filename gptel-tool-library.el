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

(defcustom gptel-tool-library-use t
  "Use tools which are safe to use")

(defcustom gptel-tool-library-use-maybe-safe nil
  "Use tools which are most likely safe to use")

(defcustom gptel-tool-library-use-unsafe nil
  "Use tools which are unsafe")

(defvar gptel-tool-library-debug t
  "Log messages to `gptel-tool-library-debug-buffer' when non-nil`")

(defvar gptel-tool-library-debug-buffer "*gptel-tool-debug*"
  "Buffer for debug output, if debug logging is enabled via `gptel-tool-library-debug'")

(defconst gptel-tool-library-dir (file-name-directory (or load-file-name
                                                          buffer-file-name)))

(defun gptel-tool-library--debug-log (log)
  "Print LOG to `gptel-tool-library-debug-buffer' if debug logging is enabled."
  (let ((buffer (get-buffer-create gptel-tool-library-debug-buffer)))
    (with-current-buffer gptel-tool-library-debug-buffer
      (insert (format "%s\n" log)))))

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
