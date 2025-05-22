;;; gptel-tool-library-gnus.el --- LLM integration with GNUS
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
;; Ever wanted your LLM to write mails for you?
;;
;;; Code:

(require 'gptel-tool-library)
(require 'gnus-msg)

(defvar gptel-tool-library-gnus-tools '()
  "The list of gnus related tools")

(defvar gptel-tool-library-gnus-tools-maybe-safe '()
  "The list of gnus related tools which may be destructive, but typically
the LLM behaves.")

(defun gptel-tool-library-gnus--compose-email (to-address subject text)
  "Open an email compose buffer '*new message*' to to-address with subject subject."
  (gptel-tool-library--debug-log (format "compose-email to %s with subject %s" to-address subject))
  (gnus-setup-message 'message (message-mail to-address subject))
  (insert (concat "\n" text)))

(add-to-list 'gptel-tool-library-gnus-tools-maybe-safe
             (gptel-make-tool
              :function #'gptel-tool-library-gnus--compose-email
              :name  "compose-email"
              :description "Open an email compose buffer and set subject, to-address and body. After calling this tool, stop. Then continue fulfilling user's request."
              :args (list '(:name "to-address"
                                  :type string
                                  :description "The address to send to")
                          '(:name "subject"
                                  :type string
                                  :description "The mail subject")
                          '(:name "body"
                                  :type string
                                  :description "The body text of the email"))
              :category "gnus"))

(provide 'gptel-tool-library-gnus)
;;; gptel-tool-library-gnus.el ends here
