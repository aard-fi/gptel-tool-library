;;; gtl-search-and-replace.el --- Tests for search and replace functionality
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
;;
;;; Code:

(require 'gptel-tool-library-search-and-replace)

(defun gtl--run-smerge-test-from-files (input-filename expected-filename search replacement)
  "Run gptel-search-replace-smerge test with files in test-data subdir.
INPUT-FILENAME and EXPECTED-FILENAME are names of files relative to test-data/.
SEARCH and REPLACEMENT are strings to use for search/replace."
  (let* ((input-file (gtl--test-data-file input-filename))
         (expected-file (gtl--test-data-file expected-filename)))
    (with-temp-buffer
      (insert-file-contents input-file)
      (gptel-search-replace-smerge search replacement)
      (let ((actual (buffer-string))
            (expected (with-temp-buffer
                        (insert-file-contents expected-file)
                        (buffer-string))))
        (should (string= actual expected)))))
  (message "Test %s -> %s passed" input-filename expected-filename))

(ert-deftest gtl--smerge1 ()
  (gtl--run-smerge-test-from-files "lorem-ipsum.txt"
                                   "smerge1.txt"
                                   "quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
                                   "quis nostrud 'aarde' exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
                                   ))

(ert-deftest gtl--smerge2 ()
  (gtl--run-smerge-test-from-files "lorem-ipsum.txt"
                                   "smerge1.txt"
                                   "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo."
                                   "Sed ut 'aarde' perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo."
                                   ))

(provide 'gtl-search-and-replace)

;;; gtl-search-and-replace.el ends here
