;;; gptel-tool-library-bbdb.el --- Tools to pull information from BBDB -*- lexical-binding: t; -*-
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
;; This module helps with information gathering from bbdb. In combination with
;; the emacs and gnus modules things like "check if somebody has a birthday
;; today and compose an email" are possible.
;;
;;; Code:

(declare-function bbdb-records "bbdb" ())
(declare-function bbdb-search "bbdb-com" (records &rest spec))

(require 'gptel-tool-library)
(eval-when-compile
  (ignore-errors
    (require 'bbdb)))

(defvar gptel-tool-library-bbdb-tools '()
  "The list of BBDB related tools")

(defun gptel-tool-library-bbdb--bbdb-search (name)
  "Search bbdb for NAME"
  (gptel-tool-library--debug-log (format "bbdb-search %s" name))
  (bbdb-search (bbdb-records) :name name))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-bbdb-tools
 :function #'gptel-tool-library-bbdb--bbdb-search
 :name  "bbdb-search"
 :description "Return a bbdb entry for name, or nil if not found. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "name"
                     :type string
                     :description "The name to search for"))
 :category "bbdb")

(defun gptel-tool-library-bbdb--bbdb-search-anniversary (anniversary-type)
  "Search bbdb for anniversary with ANNIVERSARY-TYPE"
  (gptel-tool-library--debug-log (format "search-anniversary %s" anniversary-type))
  (let ((_bbdb-default-xfield 'anniversary))
    (bbdb-search (bbdb-records) :xfield anniversary-type)))

(gptel-tool-library-make-tools-and-register
 'gptel-tool-library-bbdb-tools
 :function #'gptel-tool-library-bbdb--bbdb-search-anniversary
 :name  "bbdb-search-anniversary"
 :description "Return or a specific anniversary type. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "anniversary-type"
                     :type string
                     :description "The anniversary to search for, for example 'birthday' for birthdays"))
 :category "bbdb")

(provide 'gptel-tool-library-bbdb)
;;; gptel-tool-library-bbdb.el ends here
