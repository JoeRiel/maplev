;;; maplev-warn.el --- highlight suspicious Maple constructions

;; Copyright (C) 2017 Alexander Kobel

;; Author: Alexander Kobel <akobel@maplesoft.com>
;; Keywords: Maple, languages, faces
;; Version: 1.0

;;{{{ License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.  This program is
;; distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.  You should have received a copy of the
;; GNU General Public License along with this program; if not, write
;; to the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;;{{{ Intro
;;; Commentary:
;;
;; This package provides a minor mode warning about suspicious
;; syntax in Maple files.  It is inspired by Emacs' CWarn mode.
;;
;; Currently, it warns about the use of != for comparisons: you
;; probably meant to write <> instead, which means ``unequal'' in many
;; programming languages but which Maple interprets as two operators,
;; ``factorial (LHS) equals RHS''.

;; Usage:
;;
;; MapleV-warn is implemented as two minor modes: `maplev-warn-mode' and
;; `global-maplev-warn-mode'.  The former can be applied to individual buffers
;; and the latter to all buffers.
;;
;; Activate this package by Customize, or by placing the following line
;; into the appropriate init file:
;;
;;    (global-maplev-warn-mode 1)
;;
;; Also, `font-lock-mode' or `global-font-lock-mode' must be enabled.
;;}}}

;;; Code:

;;{{{ Dependencies

(require 'custom)
(require 'font-lock)
(require 'maplev-compat)
(require 'maplev-custom)

;;}}}
;;{{{ Custom Variables

(defgroup maplev-warn nil
  "Highlight suspicious Maple constructions."
  :version "1.0"
  :group 'maplev)

(defcustom maplev-warn-configuration
  '((maplev-mode t)
    (mpldoc-mode (not bar)))
  "List of items each describing which features are enabled for a mode.
Each item is of the form \(mode featurelist\), where featurelist can be
one of three forms:

* A list of enabled features.
* A list starting with the atom `not' followed by the features
  which are not enabled.
* The atom t, which represents that all features are enabled.

See variable `maplev-warn-font-lock-feature-keywords-alist' for available
features."
  :type '(repeat sexp)
  :group 'maplev-warn)

(defcustom maplev-warn-font-lock-feature-keywords-alist
  '((unequal . maplev-warn-font-lock-unequal-keywords))
  "An alist mapping a MapleV-warn feature to font-lock keywords.
The keywords can be either a font-lock keyword list or a symbol.
If it is a symbol it is assumed to be a variable containing a font-lock
keyword list."
  :type '(alist :key-type (choice (const unequal))
		:value-type (sexp :tag "Value"))
  :group 'maplev-warn)

(defcustom maplev-warn-verbose t
  "When nil, MapleV-warn mode will not generate any messages.

Currently, messages are generated when the mode is activated and
deactivated."
  :group 'maplev-warn
  :type 'boolean)

(defcustom maplev-warn-mode-text " MapleV-warn"
  "String to display in the mode line when MapleV-warn mode is active.

\(When the string is not empty, make sure that it has a leading space.)"
  :tag "MapleV-warn mode text"                ; To separate it from `global-...'
  :group 'maplev-warn
  :type 'string)

(defcustom maplev-warn-load-hook nil
  "Functions to run when MapleV-warn mode is first loaded."
  :tag "Load Hook"
  :group 'maplev-warn
  :type 'hook)

;;}}}
;;{{{ Modes

;;;###autoload
(define-minor-mode maplev-warn-mode
  "Minor mode that highlights suspicious Maple constructions.

Suspicious constructs are highlighted using `font-lock-warning-face'.

Note, in addition to enabling this minor mode, the major mode must
be included in the variable `maplev-warn-configuration'.  By default,
the maplev-mode is included.

With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  :group 'maplev-warn :lighter maplev-warn-mode-text
  (maplev-warn-font-lock-keywords maplev-warn-mode)
  (if font-lock-mode (font-lock-ensure)))

;;;###autoload
(define-globalized-minor-mode global-maplev-warn-mode
  maplev-warn-mode turn-on-maplev-warn-mode-if-enabled)

;;}}}
;;{{{ Helper functions

(defun turn-on-maplev-warn-mode-if-enabled ()
  "Turn on `maplev-warn-mode' in the current buffer if applicable.
The mode is turned on if any feature in `maplev-warn-configuration'
is enabled for the current `major-mode'."
  (when (maplev-warn-is-enabled major-mode)
    (maplev-warn-mode 1)))

(defun maplev-warn-is-enabled (mode &optional feature)
  "Return non-nil if MODE has FEATURE enabled.
FEATURE is an atom representing one construction to highlight.

If FEATURE is nil, check whether any is enabled for MODE.

The valid features are described by the variable
`maplev-warn-font-lock-feature-keywords-alist'."

  (let ((mode-configuration (assq mode maplev-warn-configuration)))
    (and mode-configuration
	 (or (null feature)
	     (let ((list-or-t (nth 1 mode-configuration)))
	       (or (eq list-or-t t)
		   (if (eq (car-safe list-or-t) 'not)
		       (not (memq feature (cdr list-or-t)))
		     (memq feature list-or-t))))))))

(defun maplev-warn-font-lock-keywords (addp)
  "Install/remove keywords into current buffer.
If ADDP is non-nil, install, else remove."
  (dolist (pair maplev-warn-font-lock-feature-keywords-alist)
    (let ((feature (car pair))
	  (keywords (cdr pair)))
      (if (not (listp keywords))
	  (setq keywords (symbol-value keywords)))
      (if (maplev-warn-is-enabled major-mode feature)
	  (funcall (if addp 'font-lock-add-keywords 'font-lock-remove-keywords)
		   nil keywords)))))

;;}}}
;;{{{ Inequality (!= instead of <>)

(defconst maplev-warn-font-lock-unequal-keywords
  '(("\\(!=\\)" (1 font-lock-warning-face)))
  "Match the string != in the first group.")

;;}}}

;;{{{ The end

(provide 'maplev-warn)

(run-hooks 'maplev-warn-load-hook)

;;}}}

;;; maplev-warn.el ends here
