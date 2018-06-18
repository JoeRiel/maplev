;;; ob-maple.el --- org-babel functions for Maple evaluation

;; Copyright (C) Joseph S. Riel

;; Author: Joseph S. Riel
;; Keywords: literate programming, Maple, Org Mode
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides some basic hooks to ... 

;;; Code:

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'maplev)

(add-to-list 'org-src-lang-modes '("maple" . maplev))
(add-to-list 'org-babel-tangle-lang-exts '("maple" . "mpl"))

;; FIXME:  this does not belong here (nor does it work)
;; (org-babel-do-load-languages 
;;  'org-babel-load-languages
;;  '((maple . t)
;;    (shell . t)
;;    (emacs-lisp . t)))

(defcustom org-babel-maple-command
  "maplemain -q -c 'interface(prettyprint=0)'"
  "String used to execute Maple code."
  :group 'org-babel
  :group 'maplev
  :type 'string)

(defcustom org-babel-maple-mode 'maplev-mode
  "Mode for use in running maple interactively."
  :group 'org-babel
  :group 'maplev
  :type 'symbol)

(defun org-babel-execute:maple (body params)
  "Execute a block of Maple code with org-babel.
This function is called by `org-babel-execute-src-block'.  
BODY is a string of the contents of the block.  
PARAMS is a list of cons-cells of the form \(:key . \"value\"\)."
  (unless (string= "none" (cdr (assoc :session params)))
    (error "sessions are currently not supported"))
    (org-babel-eval org-babel-maple-command
                    (org-babel-expand-body:maple body params)))

(defun org-babel-expand-body:maple (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((body (concat
               (org-babel-variable-assignments:maple params)
               "\n" body "\n")))
    (if (eq 'value (cdr (assoc :result-type params)))
        (setq body (concat
		    "try\n"
                    "    writeto(\"babel.out\"):\n"
                    body
		    "_val := %:\n"
		    "catch:\n"
		    "    _val := StringTools:-FormatMessage(lastexception[2..-1]);\n"
		    "finally\n"
                    "    writeto('terminal'):\n"
		    "end try;\n"
                    "_val;")))
    body))

(defun org-babel-maple-var-to-maple (var)
  "Convert VAR to a Maple expression."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-maple-var-to-maple var ", ") "]")
    (format "%S" (if (stringp var) (substring-no-properties var) var))))

(defun org-babel-variable-assignments:maple (params)
  "Return a string of Maple statements assigning the header variables."
  "\n")
  ;; (mapconcat
  ;;  (lambda (param)
  ;;    (format "%s := %s;"
  ;;            (cadr param)
  ;;            (org-babel-maple-var-to-maple (cddr param))))
  ;;  (org-babel-get-header params :var)
  ;;  "\n"))

(provide 'ob-maple)
;;; ob-maple.el ends here
