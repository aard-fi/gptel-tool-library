;;; gptel-tool-library-url.el --- URL based LLM bindings
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

(require 'gptel-tool-library)

(defvar gptel-tool-library-url-tools '()
  "The list of buffer turtle related tools")

(defvar gptel-tool-library-url-tools-maybe-safe '()
  "The list of buffer turtle related tools which may be destructive, but typically
the LLM behaves.")

(defvar gptel-tool-library-url-tools-unsafe '()
  "The list of buffer related related tools which are not safe.")

(defun gptel-tool-library-url--get-url (url)
  "Return the content of the URL at URL"
  (gptel-tool-library--debug-log (format "get-url %s" url))
  (let ((buffer (url-retrieve-synchronously url)))
    (when buffer
      (with-current-buffer buffer
        (message (format "LLM pulls URL %s" url))
        (concat (buffer-string))))))

(add-to-list 'gptel-tool-library-url-tools-unsafe
             (gptel-make-tool
              :function #'gptel-tool-library-url--get-url
              :name  "get-url"
              :description "rectrieve an url. after calling this tool, stop. then continue fulfilling user's request."
              :args (list '(:name "url"
                                  :type string
                                  :description "the url to retrieve."))
              :category "url"
              :confirm t))

(defun gptel-tool-library-url--get-website (url)
  "return the content of the url as rendered page."
  (gptel-tool-library--debug-log (format "get-website %s" url))
  (let ((buffer (url-retrieve-synchronously url)))
    (if  buffer
        (with-current-buffer buffer
          (message (format "llm pulls website %s" url))
          (shr-render-region (point-min) (point-max))
          (concat (buffer-substring-no-properties (point-min) (point-max))))
      "unable to load website")))

(add-to-list 'gptel-tool-library-url-tools-unsafe
             (gptel-make-tool
              :function #'gptel-tool-library-url--get-website
              :name  "get-website"
              :description "rectrieve a website and render it. after calling this tool, stop. then continue fulfilling user's request."
              :args (list '(:name "url"
                                  :type string
                                  :description "the url to retrieve."))
              :category "url"))

(provide 'gptel-tool-library-url)
;;; gptel-tool-library-url.el ends here
