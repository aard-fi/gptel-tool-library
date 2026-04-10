;;; gptel-tool-library-project.el --- Tools related to project handling -*- lexical-binding: t; -*-
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
;; This module contains some safe modules useful for gathering various data from
;; emacs. For potentially destructive tools as well as for elisp development
;; support see the `elisp' module.
;;
;;; Code:

(require 'gptel-tool-library)
(require 'project)

(defvar gptel-tool-library-project-tools '()
  "The list of emacs related tools")
;; we skip the other two variables as this module should only contain safe tools

(defun gptel-tool-library-project--current (&optional project)
  "Return the project for `project', or current directory"
  (or (and project (project-current nil project))
      (project-current)))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-project-tools
 :function #'gptel-tool-library-project--current
 :name  "project-current"
 :description "Return the root directory of a project, or nil, if not in a project."
 :args (list '(:name "project"
                     :type string
                     :description "The path to the project directory. Current working directory when omitted."
                     :optional t))
 :category "project")

(defun gptel-tool-library-project--files (&optional project dirs)
  "Return list of files in the current project.

Optional DIRS is a list of directories within the project to filter by."
  (let* ((project (or (and project (project-current nil project))
                      (project-current)))
         (project-dir (if project (project-root project) (error "Not in a project"))))
    (if (not project)
        (message "Not currently in a project")
      (let* ((files (project-files project))
             (filtered-files files))
        (when dirs
          (setq filtered-files
                (cl-remove-if-not
                 (lambda (file)
                   (let ((relpath (file-relative-name file project-dir)))
                     (cl-some (lambda (dir) (string-prefix-p dir relpath)) dirs)))
                 files)))
        filtered-files))))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-project-tools
 :function #'gptel-tool-library-project--files
 :name  "project-files"
 :description "Returns the files contained in a project, optionally limited to subdirectories."
 :args (list '(:name "project"
                     :type string
                     :description "The path to the project directory. Current working directory when omitted."
                     :optional t)
             '(:name "dirs"
                     :type array
                     :description "A list of subdirectories of the project for limiting file results. They're expected relative to the project directory, so there's no need trying to expand them."
                     :optional t))
 :category "project")

(defun gptel-tool-library-project--buffers (&optional project)
  "Return list of buffers visiting files in the current project.
Optional DIRS is a list of directories within the project to filter by."
  (let* ((project (or (and project (project-current nil project))
                      (project-current))))
    (if (not project)
        (message "Not currently in a project")
      (project-buffers project))))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-project-tools
 :function #'gptel-tool-library-project--buffers
 :name  "project-buffers"
 :description "Returns the open buffers related to a project."
 :args (list '(:name "project"
                     :type string
                     :description "The path to the project directory. Current working directory when omitted."
                     :optional t))
 :category "project")

(provide 'gptel-tool-library-project)
;;; gptel-tool-library-project.el ends here
