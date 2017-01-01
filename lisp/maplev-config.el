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

(defclass maplev-config-class ()

  ((compile
    :initarg            :compile
    :type               string
    :documentation      "Command to build and install the mla.")

   (include-path
    :initarg            :include-path
    :initform           ""
    :type               (or list string)
    :documentation 
"A string or list of strings of directories to search for files
specified with $include statements in Maple source files.")

   (maple
    :initarg            :maple
    :initform           "maple"
    :type               string
    :documentation      "Command to execute tty Maple.")

   (maple-options
    :initarg            :maple-options
    :initform           "-B -A2 -e2"
    :type               string
    :documentation      "Options to pass to tty Maple.  See :include-path.")
   
   ;; (maple-root
   ;;  :initarg            :maple-root
   ;;  :initform           (getenv "MAPLE_ROOT")
   ;;  :type               string
   ;;  :documentation      "Path to installed Maple to use.  Currently not used.")

   (mint-options
    :initarg            :mint-options
    :initform           "-q"
    :type               string
    :documentation      "Options to pass to mint.  See :include-path.")

   (project-root
    :initarg            :project-root 
    :type               string
    :documentation      "Path to root of project.  Used with mint to find files.")

   (tester
    :initarg            :tester
    :initform           "tester"
    :type               string
    :documentation      "Command to execute tester.  Used by `mpldoc-test-run-tester'")

   (tester-options
    :initarg            :tester-options
    :initform           ""
    :type               string
    :documentation      "Options to pass to tester.  Used by `mpldoc-test-run-tester'."))
  
  "A class for Maple projects.")

(defvar maplev-config (make-instance 'maplev-config-class)
  "Buffer-local variable that stores the configuration settings.
It is an instance of `maplev-config-class'.")

(make-variable-buffer-local 'maplev-config)

(defun maplev-config (&rest fields)
  "Assign the buffer-local variable `maplev-config' by passing FIELDS
to the object constructor for `maplev-config-class'.  If the slot
`:compile' is assigned, assign its value to `compile-command' which is
made buffer-local.  Unless the slot `:tester-options' is assigned,
assign it to use the values of the slots `:maple' and `:maple-options'."
  (setq maplev-config (apply #'make-instance 'maplev-config-class fields))
  ;; assign compile
  (if (slot-boundp maplev-config :compile)
      (set (make-local-variable 'compile-command) (oref maplev-config :compile)))
  ;; assign tester options
  (unless (slot-boundp maplev-config :tester-options)
    (oset maplev-config :tester-options
          (format "-maple=%s -moptions=\"%s\""
                  (oref maplev-config :maple)
                  (oref maplev-config :maple-options)))))

(defmethod maplev-get-options ((config maplev-config-class) option)
  "Catenate the OPTION slot of CONFIG, an object of type `maplev-config-class',
with an include-path option, prepended with \" -I \", unless the
`:include-path' slot of CONFIG is nil or the empty string.  If
the `:include-path' slot is a list of strings, join them with
commas separating each string."
    (concat (slot-value config option)
	    (let ((path (oref config :include-path)))
	      (unless (stringp path)
		(setq path (mapconcat 'identity path ",")))
	      (unless (string= path "")
		(concat " -I " path)))))

(provide 'maplev-config)
