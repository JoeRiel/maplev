;;; maplev-config.el -- Assign class to configure Maple
;;
;; Copyright (C) 2016
;; Author: Joseph S. Riel <jriel@maplesoft.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;     
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;     
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc.,  51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.
;;
;;; Commentary:
;;
;; Provide the function `maplev-config' and buffer-local class
;; variable `maplev-config' to record the Maple configuration
;; necessary to build a Maple archive (.mla file), run mint, 
;; and execute mpldoc tests from within a Maple source file.

(require 'eieio)
(require 'eieio-custom)
(eval-when-compile
  (defvar maplev-config-default)) ; see maplev-custom.el

(defclass maplev-config-class ()

  ((compile
    :initarg            :compile
    :initform           nil
    :type               (or null string)
    :documentation      "Command to build and install the mla.")

   (include-path
    :initarg            :include-path
    :initform           nil
    :type               (or list string)
    :custom             (repeat string)
    :documentation 
"A list of strings of directories to search for files
specified with $include statements in Maple source files.")

   (maple
    :initarg            :maple
    :initform           "maple"
    :custom             string
    :type               string
    :documentation      "Command to execute tty Maple.")

   (mapledir
    :initarg            :mapledir
    :initform           nil
    :type               (or null string)
    :documentation      "Location of Maple installation.
Same as result of kernelopts('mapledir').")

   (maple-options
    :initarg            :maple-options
    :initform           "-B -A2 -e2"
    :type               string
    :custom             string
    :documentation      "Options to pass to tty Maple.
See the Maple help page for maple.")

   (mint
    :initarg            :mint
    :initform           "mint"
    :type               string
    :custom             string
    :documentation      "Command to execute Mint.")
   
   (mint-options
    :initarg            :mint-options
    :initform           "-i2 -q -w 100 -v"
    :type               string
    :custom             string
    :documentation      "Options to pass to Mint.
See the Maple help page for mint.")

   (tester
    :initarg            :tester
    :initform           "tester"
    :type               string
    :custom             string
    :documentation      "Command to execute Maple tester.  
Used by `mpldoc-test-run-tester'.")

   (tester-options
    :initarg            :tester-options
    :initform           nil
    :type               (or null string)
    :custom             (choice (const :tag "Use default" nil) string)
    :documentation      "Options to pass to tester.
The default, nil, uses the value of the :maple slot for the -maple option
and the value of the :maple-options slot for the -moptions option.
Used by `mpldoc-test-run-tester'."))
  
  "A class for configuring Maple projects.")

(defvar maplev-config nil
  "Buffer-local variable that stores the MapleV configuration settings.
It is an instance of `maplev-config-class'.")

(make-variable-buffer-local 'maplev-config)

(defun maplev-config (&rest fields)
  "Assign the buffer-local variable `maplev-config' by passing
FIELDS to the object constructor for `maplev-config-class'. 

 If the slot `:compile' is non-nil, assign its value to
`compile-command' which is made buffer-local.  

If the slot `:mapledir' is nil, assign its value by calling Maple.

Return the object."
  (setq maplev-config (apply #'clone maplev-config-default fields))
  (let ((compile (slot-value maplev-config 'compile)))
    (when compile
      (set (make-local-variable 'compile-command) compile)))
  (let ((path (slot-value maplev-config 'include-path)))
    (when (stringp path)
      (oset maplev-config :include-path (list path))))
  (unless (slot-value maplev-config 'mapledir)
    ;; Assign the :mapledir slot
    (oset maplev-config :mapledir (shell-command-to-string
				   (format "%s -q -c 'printf(kernelopts(mapledir))' -c done"
					   (slot-value maplev-config 'maple)))))
  maplev-config)


(cl-defmethod maplev-get-option-with-include ((config maplev-config-class) option)
  "Catenate the OPTION slot of CONFIG, an object of type `maplev-config-class',
with an include-path option, prepended with \" -I \", unless the
`:include-path' slot of CONFIG is nil or the empty string.  If
the `:include-path' slot is a list of strings, join them with
commas separating each string."
    (concat (slot-value config option)
	    (let ((path (slot-value config 'include-path)))
	      (when path
		(concat " -I " (mapconcat 'identity path ","))))))

(provide 'maplev-config)
