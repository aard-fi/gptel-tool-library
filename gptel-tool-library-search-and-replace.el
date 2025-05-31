;;; gptel-tool-library-search-and-replace.el ---
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
;;
;;; History:
;;
;; This file is maintained at https://github.com/bwachter/bug-mode/
;; Check the git history for details.
;;
;;; Code:

(defun gptel-search-replace-smerge-old (search replacement &optional start end)
  "Search for SEARCH pattern and replace with REPLACEMENT string with review.
START and END define the search region, defaults to whole buffer.

Inserts a smerge conflict block with original and replacement texts for review.

Activate smerge-mode for interactive acceptance of changes."
  (interactive
   (let ((s (read-string "Search pattern: "))
         (r (read-string "Replacement string: ")))
     (list s r)))
  (save-excursion
    (let ((start (or start (point-min)))
          (end (or end (point-max)))
          (case-fold-search nil))
      (goto-char start)
      (while (re-search-forward search end t)
        (let* ((match-start (match-beginning 0))
               (match-end (match-end 0))
               (original (buffer-substring-no-properties match-start match-end)))
          ;; Remove matched text and insert conflict markers for smerge
          (goto-char match-start)
          (delete-region match-start match-end)
          (insert (format "<<<<<<< ORIGINAL\n%s\n=======\n%s\n>>>>>>> REPLACEMENT\n" original replacement))))))
  (smerge-mode 1))


(defun gptel-search-replace-smerge (search replacement &optional start end)
  "Search for SEARCH and replace with REPLACEMENT using smerge conflict blocks.
Supports multiline SEARCH and REPLACEMENT strings.
Optional region from START to END bounds the search (defaults to whole buffer)."
  (interactive
   (let ((s (read-string "Search string: "))
         (r (read-string "Replacement string: ")))
     (list s r (point-min) (point-max))))
  (save-excursion
    (let ((case-fold-search nil)
          (pos (or start (point-min)))
          (end (or end (point-max)))
          (search-len (length search))
          (search-lines (split-string search "\n"))
          (replacement-lines (split-string replacement "\n")))
      (goto-char pos)
      (while (and (< (point) end)
                  (search-forward search end t))
        (let* ((match-start (match-beginning 0))
               (match-end (match-end 0))
               (start-line-start (save-excursion (goto-char match-start) (line-beginning-position)))
               (end-line-end (save-excursion (goto-char match-end) (line-end-position)))
               (original-region (buffer-substring-no-properties start-line-start end-line-end))
               ;; Calculate prefix and suffix for the first and last lines
               (first-line-prefix (substring original-region 0 (- match-start start-line-start)))
               (last-line-suffix (substring original-region (- match-end start-line-start)))
               ;; Compose original and replacement texts with prefixes/suffixes preserved
               (original-text (concat first-line-prefix
                                      (string-join search-lines "\n")
                                      last-line-suffix))
               (replacement-text (concat first-line-prefix
                                         (string-join replacement-lines "\n")
                                         last-line-suffix)))
          ;; Replace the matched region entirely
          (delete-region start-line-start (1+ end-line-end))
          (insert (format "<<<<<<< ORIGINAL\n%s\n=======\n%s\n>>>>>>> REPLACEMENT\n"
                          original-text
                          replacement-text))
          (smerge-mode 1)
          ;; Move point after inserted block to avoid infinite loop
          (goto-char (point))))))
  (message "Done replacing with smerge conflict blocks."))

(provide 'gptel-tool-library-search-and-replace)

;;; gptel-tool-library-search-and-replace.el ends here
