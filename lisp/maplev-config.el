;;; maplev-config.el -- Assign class to configure Maple
;;
;; Copyright (C) 2016 Josephs S. Riel
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

;;; Code:

(require 'eieio)
(require 'eieio-custom)
(require 'maplev-utils)
	 
(eval-when-compile
  (defvar maplev-config-default)      ; see maplev-custom.el
  (defvar maplev-config-auto-assign)  ; ibid
  (declare-function maplev-cmaple-default-pmaple "maplev-cmaple.el"))

(defclass maplev-config-class ()

  ((bindir
    :initarg            :bindir
    :initform           nil
    :type               (or null string)
    :custom             (choice (const :tag "Auto assign") directory)
    :documentation      "Location of Maple bin directory.
Same as result of kernelopts('bindir').  If nil, this field is
auto-assigned by the function `maplev-config' if `:maple'
is properly assigned and `maplev-config-auto-assign' is non-nil.")

   (compile
    :initarg            :compile
    :initform           nil
    :type               (or null string)
    :documentation      "Command to build and install the mla.")

   (include-path
    :initarg            :include-path
    :initform           nil
    :type               (or list string)
    :custom             (repeat directory)
    :documentation
"A list of directories to search for files specified with
$include statements in Maple source files.")

   (maple
    :initarg            :maple
    :initform           nil
    :type               (or null string)
    :custom             (choice (const nil) string)
    :documentation      "Command to execute tty Maple.")

   (mapledir
    :initarg            :mapledir
    :initform           nil
    :type               (or null string)
    :custom             (choice (const :tag "Auto assign" nil) directory)
    :documentation      "Location of Maple installation.
Same as result of kernelopts('mapledir').   If nil, this field is
auto-assigned by the function `maplev-config' if `:maple'
is properly assigned and `maplev-config-auto-assign' is non-nil.")

   (pmaple
    :initarg            :pmaple
    :initform           nil
    :type               (or null string)
    :custom             (choice (const nil) file)
    :documentation "Absolute path to pmaple executable,
which is used to communicate with Maple.  This should be
installed in `user-emacs-directory'/maple/bin; on linux machines
it is pmaple, on Windows it is pmaple.exe.")

   (maple-options
    :initarg            :maple-options
    :initform           "-B -A2 -e2"
    :type               string
    :custom             string
    :documentation      "Options to pass to tty Maple.
See the Maple help page for maple.")

   (mint
    :initarg            :mint
    :initform           nil
    :type               (or null string)
    :custom             (choice (const :tag "Auto assign" nil) string)
    :documentation "Command to execute Mint.  If nil, this field
is auto-assigned by the function `maplev-config' if `:maple' is
properly assigned and `maplev-config-auto-assign' is non-nil.")
   
   (mint-options
    :initarg            :mint-options
    :initform           "-i2 -q -v"
    :type               string
    :custom             string
    :documentation "Options to pass to mint; see the Maple help
page for mint.  The verbose option (normally -v), is necessary
for mint-mode to properly display and use the path to included
file.")

   (tester
    :initarg            :tester
    :initform           nil
    :type               (or null string)
    :custom             (choice (const nil) string)
    :documentation      "Command to execute Maple tester.
This is used with `mpldoc-mode', specifically, `mpldoc-test-run-tester'.")

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
It is an instance of `maplev-config-class' and, if not explicity assigned,
is inherited from `maplev-config-default'.")

(make-variable-buffer-local 'maplev-config)

(defun maplev-config (&rest fields)
  "Assign the buffer-local variable `maplev-config'.
Pass FIELDS to the object constructor for `maplev-config-class'.
The `maplev-config-default' object is used to assign the defaults.

 If the slot `:compile' is non-nil, assign its value to
`compile-command' which is made buffer-local.

If `maplev-config-auto-assign' is non-nil, the following slots
are automatically assigned if nil.  The `:bindir' and `:mapledir'
slots are assigned by querying the Maple engine, if the `:maple'
slot is assigned and usable.  The `:mint' slot is assigned from
`:bindir' if a usable file is found.

Return the object."
  (setq maplev-config
	(if maplev-config-default
	    (apply #'clone maplev-config-default fields)
	  (apply #'make-instance 'maplev-config-class fields)))
  (let ((ext (if (or (string= system-type "windows-nt")
		     (string= system-type "cygwin"))
		 ".exe"))
	file)
    (with-slots (bindir compile include-path maple mapledir mint pmaple) maplev-config
      (when compile
	(set (make-local-variable 'compile-command) compile))
      (when (stringp include-path)
	;; convert string to list
	(setq include-path (list include-path)))
      (unless pmaple
	(setq file (maplev-cmaple-default-pmaple))
	(if (file-exists-p file) (setq pmaple file)))
      (when maplev-config-auto-assign
	(when maple
	  (unless bindir
	    (setq bindir (shell-command-to-string
			  (format "%s -q -c 'printf(kernelopts(bindir))' -c done" maple))))
	  (unless mapledir
	    (setq mapledir (shell-command-to-string
			    (format "%s -q -c 'printf(kernelopts(mapledir))' -c done" maple)))))
	(unless (or mint (not bindir))
	  (setq file (concat (file-name-as-directory (slot-value maplev-config 'bindir)) "mint" ext))
	  (if (file-exists-p file) (setq mint file))))))
  maplev-config)


;; (cl-defmethod maplev-get-option-with-include ((config maplev-config-class) slot &rest options)
;;   "Return a list of options obtained from the SLOT and `:include-path' slots of CONFIG, with OPTIONS appended.
;; Convert the option string in the OPTION slot of CONFIG, an object of type `maplev-config-class',
;; to a list of strings and append the include path from the `:include-path' slot.
;; Do the right thing if the option or include path is empty."
;;   (append
;;    (maplev-split-shell-option-string
;;     (concat (slot-value config slot)
;; 	    (let ((path (remove "" (slot-value config 'include-path))))
;; 	      (when path
;; 		(concat " -I" (mapconcat 'identity path ","))))))
;;    options))

;; Use defun, instead of cl-defmethod, to work with Emacs 24



(defun maplev-get-option-with-include (config slot &rest options)
  "Return a list of options obtained from the SLOT and `:include-path' slots of CONFIG, with OPTIONS appended.
Convert the option string in the OPTION slot of CONFIG, an object of type `maplev-config-class',
to a list of strings and append the include path from the `:include-path' slot.
Do the right thing if the option or include path is empty."
  (append
   (maplev-split-shell-option-string
    (concat (slot-value config slot)
	    (let ((path (remove "" (slot-value config 'include-path))))
	      (when path
		(concat " -I" (mapconcat 'identity path ","))))))
   options))

(provide 'maplev-config)

;;; maplev-config ends here
