;; test-init.el --- common initialization for test cases
;;
;; Copyright (c) 2010-2015 bug-mode developers
;;
;; See the AUTHORS.md file for a full list:
;; https://raw.githubusercontent.com/bwachter/bug-mode/master/AUTHORS.md
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
;;; History:
;;
;; This file is maintained at https://github.com/bwachter/bug-mode/
;; Check the git history for details.
;;
;;; Code:

(require 'cl-lib)
(require 'ert)

(defvar gtl-test-directory (file-name-directory (or load-file-name (buffer-file-name))))

(add-to-list 'load-path
             (directory-file-name
              (concat
               gtl-test-directory
               "..")))

(defun gtl--test-data-file (filename)
  "Return absolute path to test data file FILENAME in `test-data' subdir."
  (expand-file-name (concat "test-data/" filename) gtl-test-directory))

(provide 'test-init)
;;; test-init.el ends here
