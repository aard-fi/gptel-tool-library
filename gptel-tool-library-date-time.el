;;; gptel-tool-library-date-time.el --- Tools for access and manipulation of time and date -*- lexical-binding: t; -*-
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
;; This module contains some tools for accessing and manipulating time and date.
;;
;;; Code:

(require 'gptel-tool-library)

(defvar gptel-tool-library-date-time-tools '()
  "The list of emacs related tools")
;; we skip the other two variables as this module should only contain safe tools

(defun gptel-tool-library-date-time--get-date ()
  "Return the current date"
  (format-time-string "%Y-%m-%d"))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-date-time-tools
 :function #'gptel-tool-library-date-time--get-date
 :name  "get-date"
 :description "Use to get the current date in %Y-%m-%d format. After calling this tool, stop. Then continue fulfilling user's request."
 :category "date-time")

(defun gptel-tool-library-date-time--get-time ()
  "Return the current time"
  (format-time-string "%H:%M"))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-date-time-tools
 :function #'gptel-tool-library-date-time--get-time
 :name  "get-time"
 :description "Use to get the current time in %H:%M format. After calling this tool, stop. Then continue fulfilling user's request."
 :category "date-time")


(provide 'gptel-tool-library-date-time)

;;; gptel-tool-library-date-time.el ends here
