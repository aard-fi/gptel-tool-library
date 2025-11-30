;;; gptel-tool-library-emacs.el --- Simple and safe emacs information gathering tools -*- lexical-binding: t; -*-
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

(defvar gptel-tool-library-emacs-tools '()
  "The list of emacs related tools")
;; we skip the other two variables as this module should only contain safe tools

(defun gptel-tool-library-emacs--describe-variable (var)
  "Return documentation for VAR."
  (gptel-tool-library--debug-log (format "describe-variable %s" var))
  (let ((symbol (intern var)))
    (if (boundp symbol)
        (prin1-to-string (symbol-value symbol))
      (format "Variable %s is not bound. This means the variable doesn't exist. Stop. Reassess what you're trying to do, examine the situation, and continue. " var))))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-emacs-tools
 :function #'gptel-tool-library-emacs--describe-variable
 :name  "describe-variable"
 :description "Returns variable contents. After calling this tool, stop. Evaluate if the result helps. Then continue fulfilling user's request."
 :args (list '(:name "var"
                     :type string
                     :description "Variable name"))
 :category "emacs")

(defun gptel-tool-library-emacs--describe-function (fun)
  "Return documentation for FUN."
  (gptel-tool-library--debug-log (format "describe-function %s" fun))
  (let ((symbol (intern fun)))
    (if (fboundp symbol)
        (prin1-to-string (documentation symbol 'function))
      (format "Function %s is not defined." fun))))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-emacs-tools
 :function #'gptel-tool-library-emacs--describe-function
 :name  "describe-function"
 :description "Returns function description. After calling this tool, stop. Evaluate if the result helps. Then continue fulfilling user's request."
 :args (list '(:name "fun"
                     :type string
                     :description "Function name"
                     :optional t))
 :category "emacs")

(defun gptel-tool-library-emacs--get-date ()
  "Return the current date"
  (format-time-string "%Y-%m-%d"))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-emacs-tools
 :function #'gptel-tool-library-emacs--get-date
 :name  "get-date"
 :description "Use to get the current date in %Y-%m-%d format. After calling this tool, stop. Then continue fulfilling user's request."
 :category "emacs")

(defun gptel-tool-library-emacs--get-time ()
  "Return the current time"
  (format-time-string "%H:%M"))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-emacs-tools
 :function #'gptel-tool-library-emacs--get-time
 :name  "get-time"
 :description "Use to get the current time in %H:%M format. After calling this tool, stop. Then continue fulfilling user's request."
 :category "emacs")

(provide 'gptel-tool-library-emacs)
;;; gptel-tool-library-emacs.el ends here
