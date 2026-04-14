;;; eai-tool-library-buffer.el --- Tool functions for buffer access -*- lexical-binding: t; -*-
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

(require 'eai-tool-library)

(defvar eai-tool-library-buffer-tools '()
  "The list of buffer related tools")

(defvar eai-tool-library-buffer-tools-maybe-safe '()
  "The list of buffer related tools which may be destructive, but typically
the LLM behaves.")

(defvar eai-tool-library-buffer-tools-unsafe '()
  "The list of buffer related tools which are not safe.")

(defvar eai-tool-library-buffer-category-name "emacs-buffer"
  "The buffer category used for tool registration")

(defvar-local eai-tool-library-buffer--last-read-pos nil
  "Buffer local variable to track LLM read position.")

(defun eai-tool-library-buffer--filename (&optional buffer)
  "Returrn the full path of the file visited by `buffer' or current buffer"
  (buffer-file-name
   (get-buffer (if buffer
                   (format "%s" buffer)
                 (current-buffer)))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--filename
 :name  "buffer-filename"
 :description "Return the filename of a buffer, or nil if no filename is associated."
 :args (list '(:name "buffer"
                     :type string
                     :optional t
                     :description "The buffer to get the filename from. Uses the active buffer when omitted."))
 :category "emacs-buffer")

(defun eai-tool-library-buffer--read-buffer-contents (buffer)
  "Return contents of BUFFER."
  (eai-tool-library--debug-log (format "read-buffer-contents %s" buffer))
  (eai-tool-library--limit-result
   (let ((buffer (get-buffer-create buffer)))
     (with-current-buffer buffer
       (concat (buffer-substring-no-properties (point-min) (point-max)))))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--read-buffer-contents
 :name  "read-buffer-contents"
 :description "Read a buffers contents. If the buffer does not exist create it, and return an empty string. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to retrieve contents from."))
 :category "emacs-buffer")

(defun eai-tool-library-buffer--read-buffer-region (buffer from to)
  "Return contents of BUFFER region from FROM to TO."
  (eai-tool-library--debug-log (format "read-buffer-region %s %s->%s" buffer from to))
  (eai-tool-library--limit-result
   (let ((buffer (get-buffer-create buffer)))
     (with-current-buffer buffer
       (concat (buffer-substring-no-properties from to))))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--read-buffer-region
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
 :category "emacs-buffer")

;; TODO - we should also limit result here, but instead of throwing an error it'd
;;        probably bet better to do only partial reads if we'd exceed the result limit
(defun eai-tool-library-buffer--read-buffer-contents-since-last-read (buffer)
  "Return contents of BUFFER since last read, or all buffer on first read."
  (eai-tool-library--debug-log (format "read-buffer-contents-since-last-read %s" buffer))
  (with-current-buffer buffer
    (unless (local-variable-p 'eai-tool-library-buffer--last-read-pos)
      (make-local-variable 'eai-tool-library-buffer--last-read-pos)
      (setq eai-tool-library-buffer--last-read-pos (point-min)))
    (let ((_buffer (get-buffer-create buffer))
          (last-pos eai-tool-library-buffer--last-read-pos))
      (setq eai-tool-library-buffer--last-read-pos (point-max))
      (message (format "Last read %s->%s" last-pos eai-tool-library-buffer--last-read-pos))
      (concat (buffer-substring-no-properties last-pos (point-max))))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--read-buffer-contents-since-last-read
 :name  "read-buffer-contents-since-last-read"
 :description "Read content added to a buffer since last reading it. On first read, return complete buffer contents If the buffer does not exist create it, and return an empty string. This assumes buffers which only get appended to - don't try to edit a buffer read with this tool. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to retrieve contents from."))
 :category "emacs-buffer")

(defun eai-tool-library-buffer--set-buffer-pos (buffer pos)
  "Set the last read position for buffer."
  (with-current-buffer buffer
    (unless (local-variable-p 'eai-tool-library-buffer--last-read-pos)
      (make-local-variable 'eai-tool-library-buffer--last-read-pos))
    (setq eai-tool-library-buffer--last-read-pos pos)))

(defun eai-tool-library-buffer--get-buffer-pos (buffer)
  "Get the last read position for buffer."
  (with-current-buffer buffer
    (unless (local-variable-p 'eai-tool-library-buffer--last-read-pos)
      (make-local-variable 'eai-tool-library-buffer--last-read-pos)
      (setq eai-tool-library-buffer--last-read-pos (point-min)))
    eai-tool-library-buffer--last-read-pos))

(defun eai-tool-library-buffer--list-buffers (&optional arg)
  "Return list of buffers."
  (eai-tool-library--debug-log (format "list-buffers %s" (or arg "")))
  (list-buffers-noselect)
  (with-current-buffer "*Buffer List*"
    (let ((content (buffer-string)))
      content)))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--list-buffers
 :name  "list-buffers"
 :category "emacs-buffer"
 :description "List buffers open in Emacs, including file names and full paths. After using this, stop. Then evaluate which files are most likely to be relevant to the user's request."
 :category "emacs-buffer")

(defun eai-tool-library-buffer--get-in-direction (direction)
  "Return the name of the buffer in the window in DIRECTION from current window.

DIRECTION should be one of \='left, \='right, \='above, \='below, \='left-above,
\='left-below, \='right-above, or \='right-below (as symbols or strings).

If there is no window in that direction, return nil."
  (eai-tool-library--debug-log (format "buffer-in-direction %s" direction))
  (let* ((dir-sym (if (symbolp direction)
                      direction
                    (intern direction)))
         (win (window-in-direction dir-sym)))
    (when win
      (buffer-name (window-buffer win)))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--get-in-direction
 :name "get-in-direction"
 :category "emacs-buffer"
 :description
 "Given a direction (e.g. left, right, above), return the name of the buffer displayed in that window relative to the currently selected window.
 Returns nil if no such window exists."
 :args (list '(:name "direction"
                     :type string
                     :description "The direction to search, one of left, right, above, below, right-above, right-below, left-above, left-below ."))
 :category "emacs-buffer")

(defun eai-tool-library-buffer--erase-buffer (buffer)
  "Erase contents of BUFFER."
  (eai-tool-library--debug-log (format "erase-buffer %s" buffer))
  (let ((buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (erase-buffer))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools-maybe-safe
 :function #'eai-tool-library-buffer--erase-buffer
 :name  "erase-buffer"
 :description "Erase buffers contents. If the buffer does not exist create it, and return an empty string. Note: editing Elisp code in a buffer only changes the buffer text, it does NOT update the running function definitions. To make changes take effect, the user must re-evaluate the modified definitions. Do not attempt to call the modified function immediately after editing its source to verify the edit. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to erase contents in."))
 :category "emacs-buffer")

(defun eai-tool-library-buffer--buffer-size (buffer)
  "Return the size of BUFFER."
  (eai-tool-library--debug-log (format "buffer-size %s" buffer))
  (let ((buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (buffer-size))))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--buffer-size
 :name  "buffer-size"
 :description "Read a buffers contents. If the buffer does not exist create it first. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to get the size from."))
 :category "emacs-buffer")

(defun eai-tool-library-buffer--replace-region (buffer from to text)
  "Replace text in BUFFER from FROM to TO with TEXT"
  (eai-tool-library--debug-log (format "replace-region %s->%s in %s with %s" from to buffer text))
  (with-current-buffer buffer
    (delete-region from to)
    (goto-char from)
    (insert text)))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools-maybe-safe
 :function #'eai-tool-library-buffer--replace-region
 :name  "replace-region"
 :description "Replace a region in a buffer with new text. If the buffer does not exist create it, and return an empty string. Note: editing Elisp code in a buffer only changes the buffer text, it does NOT update the running function definitions. To make changes take effect, the user must re-evaluate the modified definitions. Do not attempt to call the modified function immediately after editing its source to verify the edit. After calling this tool, stop. Then continue fulfilling user's request."
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
 :category "emacs-buffer")

(defun eai-tool-library-buffer--remove-region (buffer from to)
  "Remove region from FROM to TO in buffer BUFFER"
  (eai-tool-library--debug-log (format "remove-region %s->%s from %s" from to buffer))
  (with-current-buffer buffer
    (delete-region from to)))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools-maybe-safe
 :function #'eai-tool-library-buffer--remove-region
 :name  "remove-region"
 :description "Remove a region in a buffer. If the buffer does not exist create it, and return an empty string. Note: editing Elisp code in a buffer only changes the buffer text, it does NOT update the running function definitions. To make changes take effect, the user must re-evaluate the modified definitions. Do not attempt to call the modified function immediately after editing its source to verify the edit. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to remove contents in.")
             '(:name "from"
                     :type integer
                     :description "The begin of the region.")
             '(:name "to"
                     :type integer
                     :description "The end of the region."))
 :category "emacs-buffer")

(defun eai-tool-library-buffer--insert-at (buffer at text)
  "Move point in buffer BUFFER to AT, and then insert TEXT"
  (eai-tool-library--debug-log (format "insert-at %s at %s in %s" text at buffer))
  (with-current-buffer buffer
    (goto-char (+ 1 at))
    (insert text)))

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--insert-at
 :name  "insert-at"
 :description "Insert text in a buffer at a specific location. If the buffer does not exist create it, and return an empty string. Note: editing Elisp code in a buffer only changes the buffer text, it does NOT update the running function definitions. To make changes take effect, the user must re-evaluate the modified definitions. Do not attempt to call the modified function immediately after editing its source to verify the edit. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to add contents to.")
             '(:name "at"
                     :type integer
                     :description "The point in the buffer where text should be inserted.")
             '(:name "text"
                     :type string
                     :description "The text to insert with."))
 :category "emacs-buffer")

;; the following tools directly make existing functions available
(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'get-buffer-create
 :name  "get-buffer-create"
 :description "Use get-buffer-create to create or get a buffer. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to create or retrieve."))
 :category "emacs-buffer")

(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'switch-to-buffer
 :name  "switch-to-buffer"
 :description "Use switch-to-buffer to switch to a buffer. After calling this tool, stop. Then continue fulfilling user's request."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to switch to."))
 :category "emacs-buffer")

;;; Outline / structure navigation

(defvar eai-tool-library-buffer--outline-ts-node-types
  '((emacs-lisp-mode . ("function_definition" "list"))
    (python-ts-mode . ("function_definition" "class_definition" "decorated_definition"))
    (python-mode . ("function_definition" "class_definition" "decorated_definition"))
    (c-ts-mode . ("function_definition" "struct_specifier" "enum_specator" "class_specifier"))
    (c-mode . ("function_definition" "struct_specifier" "enum_specator" "class_specifier"))
    (rust-ts-mode . ("function_item" "impl_item" "struct_item" "enum_item" "trait_item"))
    (java-ts-mode . ("class_declaration" "method_declaration" "interface_declaration" "enum_declaration"))
    (json-ts-mode . ("object" "array"))
    (yaml-ts-mode . ("block_mapping" "block_sequence"))
    (html-ts-mode . ("element" "script_element" "style_element"))
    (nix-ts-mode . ("attrset_expression" "function_expression"))
    (toml-ts-mode . ("table" "array_table"))
    (css-ts-mode . ("rule_set" "media_statement"))
    (typescript-ts-mode . ("function_declaration" "class_declaration" "method_definition" "interface_declaration"))
    (js-ts-mode . ("function_declaration" "class_declaration" "method_definition")))
  "Mapping from major mode to tree-sitter node types considered outline items.
Types listed here will be walked recursively to produce an outline.")

(defun eai-tool-library-buffer--outline-ts (buffer)
  "Walk tree-sitter nodes in BUFFER to produce outline entries.

Uses `eai-tool-library-buffer--outline-ts-node-types' to determine
which node types to descend into and report.  Returns a list of
plists with :name, :type, :from, and :to keys."
  (with-current-buffer buffer
    (let* ((modes (list major-mode))
           (mode (car modes))
           (types (or (alist-get mode eai-tool-library-buffer--outline-ts-node-types)
                      (when (string-suffix-p "-ts-mode" (symbol-name mode))
                        (alist-get (intern (string-remove-suffix "-ts-mode" (symbol-name mode)))
                                   eai-tool-library-buffer--outline-ts-node-types))))
           (root (treesit-buffer-root-node))
           (result nil))
      (when (and root types)
        (let ((stack (list root)))
          (while stack
            (let* ((node (pop stack))
                   (node-type (treesit-node-type node)))
              (cond
               ((member node-type types)
                ;; This is an outline node — record it and descend
                ;; into its children for nested items
                (let* ((name-node (or (treesit-node-child-by-field-name node "name")
                                      (treesit-node-child node 0 t)))
                       (name (when name-node
                               (treesit-node-text name-node t))))
                  (push (list :name (or name node-type)
                              :type node-type
                              :from (treesit-node-start node)
                              :to (treesit-node-end node))
                        result)
                  (dolist (child (treesit-node-children node t))
                    (push child stack))))
               ;; Not an outline type but could contain outline items
               ;; at deeper nesting levels
               (t
                (dolist (child (treesit-node-children node t))
                  (push child stack))))))))
      (nreverse result))))

(defun eai-tool-library-buffer--outline-imenu-walk (items prefix result)
  "Recursively walk imenu ITEMS, prepending PREFIX to names, appending to RESULT.
Return the final RESULT list (in reverse order, to be nreversed by caller)."
  (dolist (item items)
    (cond
     ((imenu--subalist-p item)
      (setq result
            (eai-tool-library-buffer--outline-imenu-walk
             (cdr item)
             (if prefix
                 (concat prefix "/" (car item))
               (car item))
             result)))
     (t
      (let ((name (car item))
            (pos (if (overlayp (cdr item))
                     (overlay-start (cdr item))
                   (marker-position (cdr item)))))
        (push (list :name (if prefix (concat prefix "/" name) name)
                    :type "imenu"
                    :from pos
                    :to pos)
              result)))))
  result)

(defun eai-tool-library-buffer--outline-imenu (buffer)
  "Use imenu to produce outline entries for BUFFER.

Returns a flat list of plists with :name, :type, :from, and :to keys.
Handles nested imenu entries by flattening them with hierarchical names.
The :to value for each entry is derived by using the :from of the next
entry, or the buffer end for the last entry.  Trailing whitespace is
stripped from :to boundaries for precision."
  (with-current-buffer buffer
    (let ((index (imenu-default-create-index-function))
          (result nil))
      (setq result (eai-tool-library-buffer--outline-imenu-walk index nil result))
      ;; result is in reverse push order.  Sort ascending by :from.
      (setq result (sort result
                         (lambda (a b) (< (plist-get a :from)
                                          (plist-get b :from)))))
      ;; Fill in :to values by building a new list (avoid mutation issues).
      ;; Walk backwards: each entry's :to is the trimmed position before
      ;; the next entry, or point-max for the last entry.
      (let ((next-from (point-max))
            (new-result nil))
        (dolist (entry (nreverse result))
          (let ((to-pos (save-excursion
                          (goto-char next-from)
                          (skip-chars-backward " \t\n\r")
                          (point))))
            (push (list :name (plist-get entry :name)
                        :type (plist-get entry :type)
                        :from (plist-get entry :from)
                        :to to-pos)
                  new-result)
            (setq next-from (plist-get entry :from))))
        new-result))))

(defun eai-tool-library-buffer--outline-regex (buffer)
  "Use outline-regexp and regex matching to produce outline entries for BUFFER.

This is the lowest-quality fallback, used when neither tree-sitter
nor imenu has useful data.  Returns a flat list of plists."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((result nil)
            (re (or outline-regexp
                    ;; Generic fallback: common definition patterns
                    (concat "^\\("
                            (mapconcat #'identity
                                       '("defun" "defmacro" "defvar" "defconst"
                                         "class " "def " "function " "async function"
                                         "pub fn" "fn " "impl " "struct " "enum "
                                         "interface " "type " "const "
                                         "#+\\*+" "#+\\." "^@@")
                                       "\\|")
                            "\\)"))))
        (while (re-search-forward re nil t)
          (let ((beg (line-beginning-position))
                (end (line-end-position)))
            (push (list :name (string-trim (buffer-substring-no-properties beg end))
                        :type "outline"
                        :from beg
                        :to end)
                  result)))
        (nreverse result)))))

(defun eai-tool-library-buffer--outline (&optional buffer)
  "Return a structural outline of BUFFER as a list of plists.

Each plist has :name, :type, :from, and :to keys.  Uses tree-sitter
when available, falls back to imenu, then to outline-regexp matching."
  (let* ((buffer (get-buffer-create (or buffer (current-buffer))))
         (result nil))
    ;; Try tree-sitter first
    (with-current-buffer buffer
      (when (and (fboundp #'treesit-available-p)
                 (treesit-available-p)
                 (alist-get major-mode eai-tool-library-buffer--outline-ts-node-types))
        (condition-case nil
            (when (treesit-buffer-root-node)
              (setq result (eai-tool-library-buffer--outline-ts buffer)))
          (treesit-no-parser nil))))
    ;; Fallback to imenu
    (unless result
      (with-current-buffer buffer
        (when (or imenu-generic-expression
                  (fboundp #'imenu-default-create-index-function))
          (setq result (eai-tool-library-buffer--outline-imenu buffer)))))
    ;; Last fallback: regex
    (unless result
      (setq result (eai-tool-library-buffer--outline-regex buffer)))
    result))
(eai-tool-library-make-tools-and-register
 'eai-tool-library-buffer-tools
 :function #'eai-tool-library-buffer--outline
 :name "buffer-outline"
 :description "Return a structural outline of a buffer as a list of named sections with their byte positions. Uses tree-sitter when available, falls back to imenu, then to outline-regexp matching. Each entry has :name, :type, :from, and :to keys. Useful for navigating large buffers before reading or editing specific regions."
 :args (list '(:name "buffer"
                     :type string
                     :description "The buffer to get the outline of. Uses the current buffer when omitted."
                     :optional t))
 :category "emacs-buffer")

(provide 'eai-tool-library-buffer)
;;; eai-tool-library-buffer.el ends here
