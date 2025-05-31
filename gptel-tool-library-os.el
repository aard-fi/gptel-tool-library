;;; gptel-tool-library-os.el --- OS interaction tools
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
;;; Code:

(require 'gptel-tool-library)

(defvar gptel-tool-library-os-tools '()
  "The list of OS related tools")

(defvar gptel-tool-library-os-tools-maybe-safe '()
  "The list of OS related tools which may be destructive, but typically
the LLM behaves.")

(defvar gptel-tool-library-os-tools-unsafe '()
  "The list of OS related tools which are not safe.")

(defvar gptel-tool-library-os-category-name "OS"
  "The buffer category used for tool registration")

(defun gptel-tool-library-os--run-shell (command)
  "Return output of SHELL COMMAND."
  (gptel-tool-library--debug-log (format "shell %s" command))
  (with-temp-buffer
    (shell-command command (current-buffer) nil)
    (concat
     (buffer-string))))

(add-to-list 'gptel-tool-library-os-tools-unsafe
             (add-to-list 'gptel-tools
                          (gptel-make-tool
                           :function #'gptel-tool-library-os--run-shell
                           :name  "run-shell"
                           :description "Executs a bash shell command and returns its output. After calling this tool, stop. Then continue fulfilling user's request."
                           :args (list '(:name "command"
                                               :type string
                                               :description "The shell command to execute."))
                           :category "OS"
                           :confirm t)))

(defun gptel-tool-library-os--read-file-contents (filename)
  "Return contents of FILE."
  (gptel-tool-library--debug-log (format "read-file-contents %s" filename))
  (with-temp-buffer
    (insert-file-contents (expand-file-name filename))
    (concat
     (buffer-string))))

(add-to-list 'gptel-tool-library-os-tools-maybe-safe
             (gptel-make-tool
              :function #'gptel-tool-library-os--read-file-contents
              :name  "read-file-contents"
              :description "Reads and return the contents of a specified file. After calling this tool, stop. Then continue fulfilling user's request."
              :args (list '(:name "filename"
                                  :type string
                                  :description "The filename to read."))
              :category "OS"))

(provide 'gptel-tool-library-os)
;;; gptel-tool-library-os.el ends here
