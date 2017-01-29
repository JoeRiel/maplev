;;; maplev-sb.el --- Speedbar for maplev

;; Copyright (C) 2015 Joseph S. Riel

;; Author: Joseph S. Riel <jriel@maplesoft.com>
;; Maintainer: Joseph S. Riel <jriel@maplesoft.com>
;; Created: August 2015
;; Keywords: maple speedbar
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This speedbar extension indexes the modules and procedures in a
;; Maple source file.  The display is hierarchical, a module or
;; procedure can contain modules and procedures.  Anonymous procedures
;; and their content are ignored.
;;
;; Usage:
;;
;; Add the following to .emacs
;;
;;  (add-hook 'maplev-mode-hook
;;	  (lambda ()
;;	    (require 'maplev-speedbar)))
;;
;; Use M-x customize-group maplev-speedbar to customize the extension.

;;; Code:

(require 'speedbar)
(require 'maplev-re)

;; Attach new functions to handle maplev-mode.
(add-to-list 'speedbar-dynamic-tags-function-list
	     '(maplev-sb-fetch-dynamic-tags . maplev-sb-insert-tags-list))

(speedbar-add-supported-extension '(".mpl" ".mm"))

;;; Customizations

(defgroup maplev-speedbar nil
  "Customizations for Maple speedbar"
  :group 'maplev)

(defcustom maplev-sb-defined-macros nil
  "List of Maple preprocessor macro names that are defined.
These are used to evaluate preprocessor conditionals when parsing
the source.  Embedded $define and $undef macros are also used,
but do not affect this variable."
  :group 'maplev-speedbar
  :type '(repeat string))

(defcustom maplev-sb-sort-content-flag t
  "Non-nil means alphabetically sort the displayed content of a
Maple speedbar listing.  The hierarchy is preserved."
  :type 'boolean
  :group 'maplev-speedbar)

(defcustom maplev-sb-use-markers-flag t
  "Non-nil means use markers rather than locations in `maplev-speedbar'.
Markers allow the links to work as the file is edited."
  :type 'boolean
  :group 'maplev-speedbar)

;;; Constants

(defconst maplev-sb-keyword-re
  (concat "\\_<\\("
	  "module\\|proc\\|do\\|end\\|fi\\|if\\|od\\|try\\|use"
	  "\\|^\$\\([a-z]+\\)"  ; preprocessor macro
	  "\\)\\_>")            
  "Regular expression that matches Maple keywords. Keyword is in group 1.")

(defconst maplev-sb-assign-re
  (eval-when-compile
    (concat "^\\s-*"
	    "\\(?:\\(?:export\\|local\\)\\s-+\\)?"
	    "\\(" maplev--name-re "\\)"
	    "\\(?:\\s-*::\\s-*static\\s-\\)?"
	    "\\s-*:=\\s-*"))
  "Regular expression that matches Maple assignment. Name is in group 1.")

;;; Variables

(defvar maplev-sb-markers nil
  "Buffer-local list of markers used to mark modules and procedures.
Generated by `maplev-sb-mark-defuns' if `maplev-sb-use-markers-flag' is non-nil.")

(make-variable-buffer-local 'maplev-sb-markers)

(defvar maplev-sb-stack nil
  "Stack created by `maplev-sb-mark-defuns' and used by
`maplev-sb-fetch-dynamic-tags' and `maplev-sb-get-hier'.  Each
element is either the symbol 'end, or a cons-cell, (id . tag),
where id is the identifier of a module or procedure and tag is
either a marker or point at the beginning of the keyword (module
or proc) in the buffer.")

;;; Functions

(defun maplev-sb-get-macro-name ()
  "Return the macro name at right of point."
  (unless (looking-at "\\s-*\\([_A-Za-z][_A-Za-z0-9]*\\)")
    (error "not looking at macro name"))
  (match-string-no-properties 1))

(defun maplev-sb-insert-tags-list (level lst)
  "At LEVEL level, insert LST, a generic multi-level alist.
See `speedbar-insert-generic-list'."
  (speedbar-insert-generic-list level lst
				'speedbar-tag-expand
				'speedbar-tag-find))

(defun maplev-sb-sort (alist)
  "Sort ALIST alphabetically if `maplev-sb-sort-content-flag' is non-nil."
  (if maplev-sb-sort-content-flag
      (sort alist (lambda (a b) (string< (car a) (car b))))
    alist))

(defun maplev-sb-fetch-dynamic-tags (filename)
  "Return a multi-level alist for the Maple source file FILENAME.
The created alist is passed to `speedbar-insert-generic-list' via
`maplev-sb-insert-tags-list'. If a parsing error occurs, or the
major-mode associated with FILENAME is not `maplev-mode' return
t. The returned alist has the following grammar:

  alist ::= (item*)
  item  ::= term | hier
  term  ::= (id . tag)
  hier  ::= (id tag item+)

See Info node `(speedbar)Creating a display'."
  (set-buffer (find-file-noselect filename))
  ;; verify buffer has appropriate mode
  (if (not (or (eq major-mode 'maplev-mode)
	       (eq major-mode 'mpldoc-mode)))
      t
    (condition-case err
	(progn
	  (setq maplev-sb-stack (maplev-sb-mark-defuns))
	  (maplev-sb-sort (maplev-sb-make-alist)))
      (error 
       (message (cadr err))
       t))))
		

(defun maplev-sb-mark-defuns ()
  "Add markers to modules and procedures in the current buffer,
update the buffer-local variable `maplev-sb-markers', and return a
list (a stack) of items consisting of either a cons cell, \(id . marker\),
or the symbol 'end."

  ;; null existing markers
  (if maplev-sb-markers
      (while maplev-sb-markers
	(set-marker (pop maplev-sb-markers) nil)))

  (save-excursion
    ;; Set `speedbar-tag-hierarchy-method' to nil so that 
    ;; `speedbar-create-tag-hierarchy' won't reorder the list.
    ;; Make it buffer local so that the global value is not touched.
    (set (make-local-variable 'speedbar-tag-hierarchy-method) nil)
    (set (make-local-variable 'speedbar-generic-list-group-expand-button-type) 'expandtag)
    (set (make-local-variable 'speedbar-generic-list-tag-button-type) 'statictag)
    
    (goto-char (point-min))     ; start at top of buffer
    
    (let ((point (point))
	  (macros maplev-sb-defined-macros) ; list of defined macros
	  (depth 0)
	  (ends 0) 	   ; number of ends to skip over
	  case-fold-search ; use case-sensitive searches/matching
	  cond             ; flag true if preprocessor conditional
	  ifdef-stack      ; stack to handle preprocessor conditions
	  marker           ; marker used to point to start of keyword
	  skip             ; flag set according to preprocessor conditional
	  stack            ; stack
	  state    	   ; parse-state
	  tag              ; assigned marker or point
	  id)      	   ; proc/module identifier
      ;; create stack of (id . marker) or 'end
      (while (re-search-forward maplev-sb-keyword-re nil 'noerror)
	(setq state (parse-partial-sexp point (point) nil nil state)
	      point (point))
	(unless (or (nth 3 state) (nth 4 state)) ; ignore comments and strings
	  (let ((keyword (match-string-no-properties 1))
		(beg (match-beginning 0))
		(end (match-end 0)))
	    (if (char-equal (aref keyword 0) ?$)
		;; handle preprocessor macro
		(cond 
		 ((string= keyword "$ifdef")
		  (push skip ifdef-stack)
		  (push cond ifdef-stack)
		  (unless skip
		    (setq cond (member (maplev-sb-get-macro-name) macros)
			  skip (not cond))))
		 ((string= keyword "$ifndef")
		  (push skip ifdef-stack)
		  (push cond ifdef-stack)
		  (unless skip
		    (setq cond (not (member (maplev-sb-get-macro-name) macros))
			  skip (not cond))))
		 ((string= keyword "$elif")
		  (if cond
		      (setq skip t)
		    (setq cond (member (maplev-sb-get-macro-name) macros)
			  skip (not cond))))
		 ((string= keyword "$else")
		  (setq skip cond))
		 ((string= keyword "$endif")
		  (setq cond (pop ifdef-stack)
			skip (pop ifdef-stack)))
		 ((string= keyword "$define")
		  (unless skip
		    (add-to-list 'macros (maplev-sb-get-macro-name))))
		 ((string= keyword "$undef")
		  (unless skip
		    (setq macros (delete (maplev-sb-get-macro-name) macros)))))
	      (unless skip
		(if (or (when (string= keyword "end")
			  ;; ignore optional part
			  (if (looking-at "\\s-+\\w+") 
			      (goto-char (match-end 0)))
			  t)
			(string= keyword "fi")
			(string= keyword "od"))
		    (progn 
		      ;; handle end-statement
		      (setq depth (1- depth))
		      ;;(when (< depth 2) (edebug))
		      (if (zerop ends)
			  (push 'end stack)
			(setq ends (1- ends))))
		  (setq depth (1+ depth))
		  (cond 
		   ((not (zerop ends))
		    ;; already skipping; increment ends
		    (setq ends (1+ ends)))
		   ((string= keyword "proc")
		    ;; ID := proc()
		    (save-excursion
		      (beginning-of-line)
		      (if (not (looking-at maplev-sb-assign-re))
			  ;; procedure is anonymous, skip it
			  (setq ends (1+ ends))
			(setq id (match-string-no-properties 1))
			(setq marker (if maplev-sb-use-markers-flag 
					 (set-marker (make-marker) beg)
				       beg))
		      ;; assign tag to either beg or a marker
		      (if (not maplev-sb-use-markers-flag)
			  (setq tag beg)
			(setq tag (set-marker (make-marker) beg))
			(push tag maplev-sb-markers))
		      (push (cons id tag) stack))))
		   ((string= keyword "module")
		    (setq id (if (looking-at (concat "\\s-*\\(" maplev--symbol-re "\\)"))
				 ;; module ID()
				 (match-string-no-properties 1)
			       ;; ID := module()
			       (save-excursion
				 (beginning-of-line)
				 (if (looking-at maplev-sb-assign-re)
				     (match-string-no-properties 1)
				   "Anonymous Module"))))
		    (setq marker (set-marker (make-marker) beg))
		    (push marker maplev-sb-markers)
		    (push (cons id marker) stack))
		   (t 
		    (setq ends (1+ ends))))))))))
      (cond 
       ((not (zerop depth))
	(error "statements not balanced"))
       (ifdef-stack
	(error "preprocessor conditional not terminated")))
      stack)))

(defun maplev-sb-make-alist ()
  "Convert `maplev-sb-stack' to a multi-level alist."
  (let ((cont t) elem lst)
    (while (and cont maplev-sb-stack)
      (setq elem (pop maplev-sb-stack)
	    lst (if (eq elem 'end)
		    (if (eq (car maplev-sb-stack) 'end)
			(cons (maplev-sb-make-alist) lst)
		      (cons (pop maplev-sb-stack) lst))
		  (setq cont nil)
		  (cons (car elem) (cons (cdr elem) (maplev-sb-sort lst))))))
    lst))

(provide 'maplev-speedbar)

;;; maplev-sb.el ends here