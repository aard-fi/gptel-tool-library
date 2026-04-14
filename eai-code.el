;;; eai-code.el ---  -*- lexical-binding: t; -*-
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
;; Your friendly coding agent
;;
;;; Code:

(require 's)

(defcustom eai-code-directive
  "You are a large language model living in Emacs, helping with development.

You're working on one project at a time. The current project is ${project-name} in ${project-root}. You have tools available to help you operate with projects.

You can store memories just for yourself in ${memory-file}. You should read that at the start of a new session, and keep it updated with relevant information. Make sure to keep it as compact as possible, though, so we can work with limited context.

The project planning is happening in ${planning-file}.
"
  "The template for the default directive to use in eai-code chat buffers."
  :type 'string
  :group 'eai-code)

(defcustom eai-code-file-map
  '((memory   . "memory.org")
    (chat     . "chat.org")
    (planning . "planning.org"))
  "The org files, relative to the project directory."
  :type '(alist :key-type symbol :value-type string)
  :group 'eai-code)

(defcustom eai-code-default-memory
  "* LLM memory file

This file contains your memory for this project. This file is fully under your control - you can do whatever you want with it."
  "The default content of new memory files"
  :type 'string
  :group 'eai-code)

(defcustom eai-code-default-planning
  "* Planning for ${project-name}

A blank canvas so far.
"
  "The default content of new memory files"
  :type 'string
  :group 'eai-code)

(defcustom eai-code-default-backend ""
  "The name of the default backend to use for code sessions"
  :type 'string
  :group 'eai-code)

(defcustom eai-default-model nil
  "The name of the default model for code sessions"
  :type 'symbol
  :group 'eai-code)

(defun eai-code--aget (key data)
  "Safe aget for s-format.

For missing keys return the key name in s-format syntax."
  (or (alist-get key data nil nil #'string=)
      (format "${%s}" key)))

(defun eai-code--directive ()
  "Dynamically build the system prompt for the code session.

This includes details about the project, but should also give us options about
dynamically adding in information to improve results later on."
  (let ((project (project-current)))
    (s-format eai-code-directive 'eai-code--aget
              `(("project-name" . ,(project-name project))
                ("project-root" . ,(project-root project))
                ("memory-file" . ,(eai-code--get-special-file 'memory))
                ("planning-file" . ,(eai-code--get-special-file 'planning))
                ))))

(defun eai-code--get-special-file (key)
  "Return the absolute path for KEY based on `eai-code-file-map'."
  (let* ((project (project-current t))
         (root (project-root project))
         (filename (alist-get key eai-code-file-map)))
    (if filename
        (expand-file-name filename root)
      (error "No filename mapped for key: %s" key))))

(defun eai-code--ensure-directory (dir)
  "Ensure DIR exists. Create it if missing, or abort with `user-error'."
  (interactive "DTarget directory: ")
  (unless (file-directory-p dir)
    (let ((choice (completing-read
                   (format "Directory %s does not exist. Choose: " dir)
                   '("create directory" "abort") nil t)))
      (cond
       ((equal choice "create directory")
        (make-directory dir t)
        (message "Created directory: %s" dir))
       ((equal choice "abort")
        (user-error "Directory creation aborted by user")))))
  dir)

;; this selection is a bit annoying - maybe use transient instead?
(defun eai-code--find-or-create-project (&optional dir)
  "Ensure `default-directory' is in a project. If DIR is given, switch to it first.
If not in a project, offer to cd elsewhere or init a Git repo. Aborts on user cancel."
  (interactive)
  (let ((dir (or dir default-directory)))
    (let ((project (project-current nil dir)))
      (if project
          (let ((project-root (project-root project)))
            (message "✓ In project: %s" project-root)
            (setq default-directory project-root))
        (let* ((choice (completing-read
                        "Not in a project. Choose: "
                        '("change directory"
                          "initialize git repo here"
                          "abort") nil t))
               (cmd (lambda (fn)
                      (funcall fn)            ; ensure function executes and exits
                      (eai-code--find-or-create-project)))) ; tail-call again
          (cond
           ((equal choice "change directory")
            (let ((new-dir (read-directory-name "Choose project directory: ")))
              (unless (file-directory-p new-dir)
                (eai-code--ensure-directory new-dir))
              (eai-code--find-or-create-project new-dir)))
           ((equal choice "initialize git repo here")
            (setq default-directory dir)
            (unless (file-directory-p dir)
              (eai-code--ensure-directory dir))
            (vc-create-repo 'Git)
            (message "Initialized Git repo in: %s" default-directory)
            (eai-code--find-or-create-project))
           ((equal choice "abort")
            (user-error "User aborted project setup"))))))))

(defun eai-code--setup-memory ()
  "Locate the memory file, and create it, if it is empty"
  (interactive)
  (let* ((project (project-current))
         (project-root (project-root project))
         (memory-file (eai-code--get-special-file 'memory)))
    (with-current-buffer (find-file-noselect memory-file)
      (when (= (buffer-size) 0)
        (insert eai-code-default-memory)
        (save-buffer))
      (kill-buffer))))

(defun eai-code--setup-plan ()
  "Locate the planning file, and create it, if it is empty"
  (interactive)
  (let* ((project (project-current))
         (project-root (project-root project))
         (planning-file (eai-code--get-special-file 'planning)))
    (with-current-buffer (find-file-noselect planning-file)
      (when (= (buffer-size) 0)
        (let ((insert
               (s-format eai-code-default-planning 'eai-code--aget
                         `(("project-name" . ,(project-name project))
                           ("project-root" . ,(project-root project))
                           ))))
          (insert insert)
          (save-buffer)))
      (kill-buffer))))

(defun eai-code--find-chat ()
  "Locate the chat file for this project, and set it up as gptel session."
  (interactive)
  (let* ((project (project-current))
         (project-root (project-root project))
         (chat-file (eai-code--get-special-file 'chat))
         (chat-buf (find-file-noselect chat-file)))
    (with-current-buffer chat-buf
      (make-local-variable 'gptel-prompt-prefix-alist)
      (make-local-variable 'gptel-response-prefix-alist)

      (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@developer:\n")
      (setf (alist-get 'org-mode gptel-response-prefix-alist) "@code monkey:\n")
      (setq-local gptel-directives '((default . eai-code--directive)))
      (setq-local gptel--system-message (alist-get 'default gptel-directives))
      (setq-local buffer-auto-save-file-name nil)

      ;; insert prefix for new buffers
      (when (= (buffer-size) 0)
        (insert (alist-get 'org-mode gptel-prompt-prefix-alist))
        (insert "\n"))

      (gptel-mode))
    (switch-to-buffer chat-buf)))

(defun eai-code (&optional arg)
  "Start a new code agent session.

Can be called with a prefix argument to provide a directory, otherwise it'll use
the current directory, if it is a project, or prompt."
  (interactive "P")
  (let ((project-dir (when arg
                       (read-directory-name "Select project directory: "))))
    (eai-code--find-or-create-project project-dir)
    (eai-code--setup-memory)
    (eai-code--setup-plan)
    (eai-code--find-chat)))

(provide 'eai-code)

;;; eai-code.el ends here
