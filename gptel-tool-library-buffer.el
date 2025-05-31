;;; gptel-tool-library-buffer.el --- Tool functions for buffer access
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
;; Provides LLM friendly interactions with emacs buffers.
;;
;;; Code:

(require 'gptel-tool-library)

(defvar gptel-tool-library-buffer-tools '()
  "The list of buffer related tools")

(defvar gptel-tool-library-buffer-tools-maybe-safe '()
  "The list of buffer related tools which may be destructive, but typically
the LLM behaves.")

(defvar gptel-tool-library-buffer-tools-unsafe '()
  "The list of buffer related tools which are not safe.")

(defun gptel-tool-library-buffer--read-buffer-contents (buffer)
  "Return contents of BUFFER."
  (gptel-tool-library--debug-log (format "read-buffer-contents %s" buffer))
  (let ((buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (concat (buffer-substring-no-properties (point-min) (point-max))))))

(add-to-list 'gptel-tool-library-buffer-tools
             (gptel-make-tool
              :function #'gptel-tool-library-buffer--read-buffer-contents
              :name  "read-buffer-contents"
              :description "Read a buffers contents. If the buffer does not exist create it, and return an empty string. After calling this tool, stop. Then continue fulfilling user's request."
              :args (list '(:name "buffer"
                                  :type string
                                  :description "The buffer to retrieve contents from."))
              :category "emacs-buffer"))

(defun gptel-tool-library-buffer--read-buffer-region (buffer from to)
  "Return contents of BUFFER region from FROM to TO."
  (gptel-tool-library--debug-log (format "read-buffer-region %s %s->%s" buffer from to))
  (let ((buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (concat (buffer-substring-no-properties from to)))))

(add-to-list 'gptel-tool-library-buffer-tools
             (gptel-make-tool
              :function #'gptel-tool-library-buffer--read-buffer-region
              :name  "read-buffer-region"
              :description "Read a region of a buffer. If the buffer does not exist create it, and return an empty string. After calling this tool, stop. Then continue fulfilling user's request."
              :args (list '(:name "buffer"
                                  :type string
                                  :description "The buffer to retrieve contents from.")
                          '(:name "from"
                                  :type integer
                                  :description "Start of the region to read.")
                          '(:name "to"
                                  :type integer
                                  :description "End of the region to read."))
              :category "emacs-buffer"))

(defun gptel-tool-library-buffer--read-buffer-contents-since-last-read (buffer)
  "Return contents of BUFFER since last read, or all buffer on first read."
  (gptel-tool-library--debug-log (format "read-buffer-contents-since-last-read %s" buffer))
  (with-current-buffer buffer
    (unless (local-variable-p 'gptel-tool-library-buffer--last-read-pos)
      (make-local-variable 'gptel-tool-library-buffer--last-read-pos)
      (setq gptel-tool-library-buffer--last-read-pos (point-min)))
    (let ((buffer (get-buffer-create buffer))
          (last-pos gptel-tool-library-buffer--last-read-pos))
      (setq gptel-tool-library-buffer--last-read-pos (point-max))
      (message (format "Last read %s->%s" last-pos gptel-tool-library-buffer--last-read-pos))
      (concat (buffer-substring-no-properties last-pos (point-max))))))

(add-to-list 'gptel-tool-library-buffer-tools
             (gptel-make-tool
              :function #'gptel-tool-library-buffer--read-buffer-contents-since-last-read
              :name  "read-buffer-contents-since-last-read"
              :description "Read content added to a buffer since last reading it. On first read, return complete buffer contents If the buffer does not exist create it, and return an empty string. This assumes buffers which only get appended to - don't try to edit a buffer read with this tool. After calling this tool, stop. Then continue fulfilling user's request."
              :args (list '(:name "buffer"
                                  :type string
                                  :description "The buffer to retrieve contents from."))
              :category "emacs-buffer"))

(defun gptel-tool-library-buffer--set-buffer-pos (buffer pos)
  "Set the last read position for buffer."
  (with-current-buffer buffer
    (unless (local-variable-p 'gptel-tool-library-buffer--last-read-pos)
      (make-local-variable 'gptel-tool-library-buffer--last-read-pos))
    (setq gptel-tool-library-buffer--last-read-pos pos)))

(defun gptel-tool-library-buffer--get-buffer-pos (buffer)
  "Get the last read position for buffer."
  (with-current-buffer buffer
    (unless (local-variable-p 'gptel-tool-library-buffer--last-read-pos)
      (make-local-variable 'gptel-tool-library-buffer--last-read-pos)
      (setq gptel-tool-library-buffer--last-read-pos (point-min)))
    gptel-tool-library-buffer--last-read-pos))

(defun gptel-tool-library-buffer--list-buffers (&optional arg)
  "Return list of buffers."
  (gptel-tool-library--debug-log (format "list-buffers %s" (or arg "")))
  (list-buffers-noselect)
  (with-current-buffer "*Buffer List*"
    (let ((content (buffer-string)))
      content)))

(add-to-list 'gptel-tool-library-buffer-tools
             (gptel-make-tool
              :function #'gptel-tool-library-buffer--list-buffers
              :name  "list-buffers"
              :category "emacs-buffer"
              :description "List buffers open in Emacs, including file names and full paths. After using this, stop. Then evaluate which files are most likely to be relevant to the user's request."
              :category "emacs-buffer"))

(defun gptel-tool-library-buffer--erase-buffer (buffer)
  "Erase contents of BUFFER."
  (gptel-tool-library--debug-log (format "erase-buffer %s" buffer))
  (let ((buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (erase-buffer))))

(add-to-list 'gptel-tool-library-buffer-tools-maybe-safe
             (gptel-make-tool
              :function #'gptel-tool-library-buffer--erase-buffer
              :name  "erase-buffer"
              :description "Erase buffers contents. If the buffer does not exist create it, and return an empty string. After calling this tool, stop. Then continue fulfilling user's request."
              :args (list '(:name "buffer"
                                  :type string
                                  :description "The buffer to erase contents in."))
              :category "emacs-buffer"))

(defun gptel-tool-library-buffer--buffer-size (buffer)
  "Return the size of BUFFER."
  (gptel-tool-library--debug-log (format "buffer-size %s" buffer))
  (let ((buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (buffer-size))))

(add-to-list 'gptel-tool-library-buffer-tools
             (gptel-make-tool
              :function #'gptel-tool-library-buffer--buffer-size
              :name  "buffer-size"
              :description "Read a buffers contents. If the buffer does not exist create it first. After calling this tool, stop. Then continue fulfilling user's request."
              :args (list '(:name "buffer"
                                  :type string
                                  :description "The buffer to get the size from."))
              :category "emacs-buffer"))

(defun gptel-tool-library-buffer--replace-region (buffer from to text)
  "Replace text in BUFFER from FROM to TO with TEXT"
  (gptel-tool-library--debug-log (format "replace-region %s->%s in %s with %s" from to buffer text))
  (with-current-buffer buffer
    (delete-region from to)
    (goto-char from)
    (insert text)))

(add-to-list 'gptel-tool-library-buffer-tools-maybe-safe
             (gptel-make-tool
              :function #'gptel-tool-library-buffer--replace-region
              :name  "replace-region"
              :description "Replace a region in a buffer with new text. If the buffer does not exist create it, and return an empty string. After calling this tool, stop. Then continue fulfilling user's request."
              :args (list '(:name "buffer"
                                  :type string
                                  :description "The buffer to replace contents in.")
                          '(:name "from"
                                  :type integer
                                  :description "The begin of the region.")
                          '(:name "to"
                                  :type integer
                                  :description "The end of the region.")
                          '(:name "text"
                                  :type string
                                  :description "The text to replace the region with."))
              :category "emacs-buffer"))

(defun gptel-tool-library-buffer--remove-region (buffer from to)
  "Remove region from FROM to TO in buffer BUFFER"
  (gptel-tool-library--debug-log (format "remove-region %s->%s from %s" from to buffer))
  (with-current-buffer buffer
    (delete-region from to)))

(add-to-list 'gptel-tool-library-buffer-tools-maybe-safe
             (gptel-make-tool
              :function #'gptel-tool-library-buffer--remove-region
              :name  "remove-region"
              :description "Remove a region in a buffer with new text. If the buffer does not exist create it, and return an empty string. After calling this tool, stop. Then continue fulfilling user's request."
              :args (list '(:name "buffer"
                                  :type string
                                  :description "The buffer to remove contents in.")
                          '(:name "from"
                                  :type integer
                                  :description "The begin of the region.")
                          '(:name "to"
                                  :type integer
                                  :description "The end of the region."))
              :category "emacs-buffer"))

(defun gptel-tool-library-buffer--insert-at (buffer at text)
  "Move point in buffer BUFFER to AT, and then insert TEXT"
  (gptel-tool-library--debug-log (format "insert-at %s at %s in %s" text at buffer))
  (with-current-buffer buffer
    (goto-char (+ 1 at))
    (insert text)))

(add-to-list 'gptel-tool-library-buffer-tools
             (gptel-make-tool
              :function #'gptel-tool-library-buffer--insert-at
              :name  "insert-at"
              :description "At text in a buffer at a specific location. If the buffer does not exist create it, and return an empty string. After calling this tool, stop. Then continue fulfilling user's request."
              :args (list '(:name "buffer"
                                  :type string
                                  :description "The buffer to add contents to.")
                          '(:name "at"
                                  :type integer
                                  :description "The point in the buffer where text should be inserted.")
                          '(:name "text"
                                  :type string
                                  :description "The text to insert with."))
              :category "emacs-buffer"))

;; the following tools directly make existing functions available
(add-to-list 'gptel-tool-library-buffer-tools
             (gptel-make-tool
              :function #'get-buffer-create
              :name  "get-buffer-create"
              :description "Use get-buffer-create to create or get a buffer. After calling this tool, stop. Then continue fulfilling user's request."
              :args (list '(:name "buffer"
                                  :type string
                                  :description "The buffer to create or retrieve."))
              :category "emacs-buffer"))

(add-to-list 'gptel-tool-library-buffer-tools
             (gptel-make-tool
              :function #'switch-to-buffer
              :name  "switch-to-buffer"
              :description "Use switch-to-buffer to switch to a buffer. After calling this tool, stop. Then continue fulfilling user's request."
              :args (list '(:name "buffer"
                                  :type string
                                  :description "The buffer to switch to."))
              :category "emacs-buffer"))

(provide 'gptel-tool-library-buffer)
;;; gptel-tool-library-buffer.el ends here
