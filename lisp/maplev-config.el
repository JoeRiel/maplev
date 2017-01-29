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
    :initform           ""
    :type               string
    :custom             string
    :documentation      "Options to pass to Mint.
See the Maple help page for mint.")

   (project-root
    :initarg            :project-root 
    :initform           nil
    :type               (or null string)
    :documentation      "Path to root of project.  
Starting point from which to find source files when locating mint warnings.")

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
  "Assign the buffer-local variable `maplev-config' by passing FIELDS
to the object constructor for `maplev-config-class'.  If the slot
`:compile' is non-nil, assign its value to `compile-command' which is
made buffer-local.  Return the object."
  (setq maplev-config (apply #'clone maplev-config-default fields))
  (let ((compile (oref maplev-config :compile)))
    (when compile
      (set (make-local-variable 'compile-command) compile)))
  (let ((path (oref maplev-config :include-path)))
    (when (stringp path)
      (oset maplev-config :include-path (list path))))
  maplev-config)

(defmethod maplev-get-option-with-include ((config maplev-config-class) option)
  "Catenate the OPTION slot of CONFIG, an object of type `maplev-config-class',
with an include-path option, prepended with \" -I \", unless the
`:include-path' slot of CONFIG is nil or the empty string.  If
the `:include-path' slot is a list of strings, join them with
commas separating each string."
    (concat (slot-value config option)
	    (let ((path (oref config :include-path)))
	      (when path
		(concat " -I " (mapconcat 'identity path ","))))))

(provide 'maplev-config)
