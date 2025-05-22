;;; gptel-tool-library-elisp.el --- Functions to help with elisp development
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
;; This provides LLM access to some information for efficient automated elisp
;; editing. This is building on the buffer module - and generally should be
;; doable with just the buffer module, but especially LLMs with a small context
;; window might prefer having specialised tools operating on smaller context.
;;
;;; Code:

(require 'gptel-tool-library)

(defvar gptel-tool-library-elisp-tools '()
  "The list of elisp related tools")

(defvar gptel-tool-library-elisp-tools-maybe-safe '()
  "The list of elisp related tools which may be destructive, but typically
the LLM behaves.")

(defvar gptel-tool-library-elisp-tools-unsafe '()
  "The list of buffer related tools which are not safe.")

(defun gptel-tool-library-elisp--run-eval (command)
  "Return output of elisp eval COMMAND."
  (gptel-tool-library--debug-log (format "eval %s" command))
  (eval (if (stringp command)
            (car (read-from-string (format "(progn %s)" command)))
          command)))

(add-to-list 'gptel-tool-library-elisp-tools-unsafe
             (gptel-make-tool
              :function #'gptel-tool-library-elisp--run-eval
              :name  "eval"
              :description "Use eval to evaluate elisp code. After calling this tool, stop. Then continue fulfilling user's request."
              :args (list '(:name "command"
                                  :type string
                                  :description "The elisp code to evaluate."))
              :category "elisp"
              :confirm t))

(provide 'gptel-tool-library-elisp)
;;; gptel-tool-library-elisp.el ends here
