;; maplev.el --- Maple mode for GNU Emacs
;;
;; Copyright (C) 2001,2003,2008,2009,2015,2020 Joseph S. Riel

;; Authors:    Joseph S. Riel <jriel@maplesoft.com>
;;             and Roland Winkler <Roland.Winkler@physik.uni-erlangen.de>
;; Created:    June 1999
;; Version:    3.0.5
;; Keywords:   Maple, languages

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
;;{{{ Introduction

;;; Commentary:
;;
;; This package defines five major modes:
;;
;;   maplev-mode:        for editing Maple code
;;   maplev-cmaple-mode: for running Maple
;;   maplev-mint-mode:   for displaying the output of mint
;;   maplev-help-mode:   for displaying Maple help pages
;;   maplev-view-mode:   for displaying Maple procedures

;;; Features:

;; font-lock (highlighting) of Maple keywords
;; automatic indentation
;; syntax checking (via Mint)
;; online Maple help
;; online display of Maple procedures
;; outlining (not yet)
;; narrowing (nothing here)
;; tags
;; imenu support
;; auto-fill support

;;; Installation:

;; Put this file into your Emacs load path and byte compile it.  Add
;; the following to your `.emacs':
;;
;;   (autoload 'maplev-mode "maplev" "Maple editing mode" t)
;;   (autoload 'cmaple      "maplev" "Start maple process" t)
;;
;; To have Emacs automagically start in MapleV mode when editing Maple
;; source, add the following to your .emacs, modifying the regexp
;; `.mpl' to an extension appropriate for your usage:
;;
;;   (setq auto-mode-alist (cons `("\\.mpl\\'" . maplev-mode) auto-mode-alist))
;;
;; YOU MUST customize some of the default settings to be appropriate
;; for your installation.  You can do this in several ways.  The most
;; user friendly way is to use `customize'.  You can do this with:
;;
;;   M-x load-library RET maplev RET
;;   M-x customize-group RET maplev RET
;;
;; The important options are in the subgroup `maplev-important'.  After
;; setting and testing these options, save them to your .emacs by
;; clicking on the `Save for Future Sessions' button.
;;
;;
;;; History:

;; Oct 99:  Initial release.

;;}}}
;;{{{ To Do

;; High Priority:
;; - make `maplev-beginning-of-proc' and `maplev-end-of-proc' more reliable.
;;
;; Medium Priority:
;; - add comment-out functions
;; - pass `maplev-beginning-of-proc' (or faster) to `font-lock-defaults'.
;;   That should speed up fontification with lazy(?) lock.  Testing.
;; - add clean up routine to kill buffers and processes
;;   when exiting maplev-mode
;; - indent continued assignments (this could be tricky)
;; - more complete definition of maplev-completions based on
;;   the maple help node `index[package]'
;;
;; Low Priority:
;; - font lock local variables
;; - fix problem with folding

;;}}}

;;; Code:

;;{{{ Requirements

(require 'button-lock)

(require 'comint)
(require 'font-lock)
(require 'imenu)
(require 'info)

(require 'maplev-compat)                ; compatibility definitions for older Emacs
(require 'maplev-cmaple)                ; interact with Maple
(require 'maplev-common)                ; common functions
(require 'maplev-config)                ; configure maple/mint/tester
(require 'maplev-custom)                ; customizable variables
(require 'maplev-help)                  ; maplev-help-mode (view help pages)
(require 'maplev-indent)                ; indentation engine
(require 'maplev-mint)                  ; maplev-mint-mode (view mint output)
(require 'maplev-re)                    ; regular expressions
(require 'maplev-speedbar "maplev-sb")  ; speedbar for maple source
(require 'maplev-trace)                 ; functions for indenting trace output
(require 'maplev-utils)                 ; not much here just yet
(require 'maplev-version)               ; assign version
(require 'maplev-view)                  ; maplev-view-mode (view procedures)
(require 'maplev-warn)

;;}}}

;;{{{ Information

(defconst maplev-developer
  "Joseph S. Riel <jriel@maplesoft.com>"
  "Developer/maintainer of `maplev-mode'.")

;;}}}

;;{{{ Version

;; Reassign the functions maplev-release and maplev-git-release
;; if the file maplev-release.el is available.

(let* ((maplev-dir (file-name-directory (or (locate-library "maplev") "")))
       (maplev-release.el (concat maplev-dir "maplev-release.el")))
  (when (require 'maplev-release  "maplev-release" 'noerror)
    (autoload 'maplev-release     "maplev-release")
    (autoload 'maplev-git-release "maplev-release")))

;;;###autoload
(defun maplev-version (&optional here full message)
  "Show the `maplev-mode' version in the echo area.
With prefix argument HERE, insert it at point.
When FULL is non-nil, use a verbose version string.
When MESSAGE is non-nil, display a message with the version."
  (interactive (list current-prefix-arg t (not current-prefix-arg)))
  (let* ((maplev-version (maplev-release))
	 (git-version (maplev-git-version))
	 (version (if full
		      (format "Maplev-mode version %s (%s)"
			      maplev-version
			      git-version)
		    maplev-version)))
    (when here (insert version))
    (when message (message "%s" version))
    version))

;;}}}

(eval-and-compile
  (condition-case nil (require 'imenu) (error nil))
  (condition-case nil (require 'align) (error nil)))

(defsubst maplev--short-delay ()
  "Pause for a brief duration."
  (sleep-for 0.1))


;;{{{ Internal variables

(defvar maplev-completions nil
  "List for minibuffer completion.")

;;}}}
;;{{{ Syntax table

(defvar maplev-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_  "_"  table) ; symbol constituent
    (modify-syntax-entry ?~  "_"  table) ; symbol constituent
    (modify-syntax-entry ??  "_"  table) ; symbol constituent
    (modify-syntax-entry ?&  "w"  table) ; word constituent
    (modify-syntax-entry ?%  "w"  table) ; word constituent (questionable; symbol?)

    (modify-syntax-entry ?\\ "\\" table) ; escape
    (modify-syntax-entry ?#  "<"  table) ; comment starter
    (modify-syntax-entry ?\n ">"  table) ; newline = comment ender
    (modify-syntax-entry ?\f ">"  table) ; formfeed = comment ender
    (modify-syntax-entry ?\r " "  table) ; return = whitespace
    (modify-syntax-entry ?\t " "  table) ; tab = whitespace

    (modify-syntax-entry ?*  ". 23b"  table) ; punctuation and used in multiline comments (* ... *)

    (modify-syntax-entry ?/  "."  table)
    (modify-syntax-entry ?+  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?>  "."  table)
    (modify-syntax-entry ?<  "."  table)
    (modify-syntax-entry ?.  "."  table)
    (modify-syntax-entry ?|  "."  table)
    
    (modify-syntax-entry ?\" "\"" table) ; string quote
    (modify-syntax-entry ?\' "\"" table) ; string quote
    (modify-syntax-entry ?\` "\"" table) ; string quote
    (modify-syntax-entry ?\{ "(}" table) ; balanced brackets
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\( "()1n" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\) ")(4n" table)

    ;; Entries for R5 and later
    (modify-syntax-entry ?\" "\"" table)

    table)
  "Syntax table used in MapleV mode buffers.")

(defvar maplev-mode-4-syntax-table
  (let ((table (make-syntax-table maplev-mode-syntax-table)))
    (modify-syntax-entry ?\" "." table)
    table)
  "Syntax table used in MapleV mode buffers for R4.")

(defvar maplev-symbol-syntax-table
  (let ((table (make-syntax-table maplev-mode-syntax-table)))
    (modify-syntax-entry ?_  "w"  table)
    table)
  "Syntax table for Maple, where `_' is a word constituent.")

(defvar maplev-help-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table used in Maple help buffer.")

(defvar maplev-quote-not-string-syntax-table
  (let ((table (make-syntax-table maplev-symbol-syntax-table)))
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?\` "_" table)
    table)
  "Syntax table used by `maplev--re-search-forward'.")


;;}}}

;;{{{ Mode map

(defvar maplev-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(tab)]                      'maplev-electric-tab)
    (define-key map [(meta tab)]                 'maplev-complete-symbol)
    (define-key map [(control c) (meta tab)]     'maplev-add-exports-of-module-at-point)
    (define-key map [(backspace)]                'backward-delete-char-untabify)
    (define-key map [(control backspace)]        'maplev-untab)
    (define-key map [(control ?\;)]              'maplev-insert-assignment-operator)
    (define-key map [(control c) (control t) ?p] 'maplev-template-proc)
    (define-key map [(control c) (control t) ?m] 'maplev-template-module)
    (define-key map [(control c) (control t) ?u] 'maplev-template-use-statement)

    (define-key map [(control j)]                'maplev-indent-newline)
    (define-key map [(control return)]           'maplev-newline-and-comment)
    (define-key map [(meta control h)]           'maplev-mark-defun)
    ;;  (define-key map [(meta control a)]           'maplev-beginning-of-proc)
    ;;  (define-key map [(meta control e)]           'maplev-end-of-proc)
    (define-key map [(control x) ?n ?d]          'maplev-narrow-to-defun)


    (when (fboundp 'x-get-cut-buffer)
      ;; These two bindings are needed only under linux / unix
      (define-key map [(meta control y)]          'maplev-insert-cut-buffer)
      (define-key map [(control meta mouse-2)]    'maplev-mouse-yank-cut-buffer))

    (define-key map [(control c) (control l)] 'maplev-add-local-variable)
    (define-key map [(control c) (control g)] 'maplev-add-global-variable)
    (define-key map [(control c) (control e)] 'maplev-add-export-variable)

    ;; Indent commands
    (define-key map [(control c) (tab) ?b]  'maplev-indent-buffer)
    (define-key map [(control c) (tab) tab] 'maplev-indent-buffer)
    (define-key map [(control c) (tab) ?p]  'maplev-indent-procedure)
    (define-key map [(control c) (tab) ?r]  'maplev-indent-region)
    (define-key map [(control c) (tab) ?k]  'maplev-indent-clear-info)
    
    ;; Cmaple commands
    (define-key map [(control c) (control c) ?b]      'maplev-cmaple-send-buffer)
    (define-key map [(control c) (control c) ?p]      'maplev-cmaple-send-procedure)
    (define-key map [(control c) (control c) ?r]      'maplev-cmaple-send-region)
    (define-key map [(control c) (control c) ?l]      'maplev-cmaple-send-line)
    (define-key map [(control c) (control c) return]  'maplev-cmaple-send-line)
    (define-key map [(control c) (control c) ?g]      'maplev-cmaple-pop-to-buffer)
    (define-key map [(control c) (control c) ?i]      'maplev-cmaple-interrupt)
    (define-key map [(control c) (control c) ?k]      'maplev-cmaple-kill)
    (define-key map [(control c) (control c) ?s]      'maplev-cmaple-status)

    ;; Mint commands
    (define-key map [(control c) return ?b] 'maplev-mint-buffer)
    (define-key map [(control c) return ?p] 'maplev-mint-procedure)
    (define-key map [(control c) return ?r] 'maplev-mint-region)
    (define-key map [(control c) return return] 'maplev-mint-rerun)

    ;; Help and proc comma
    (define-key map [(control ?\?)] 'maplev-help-at-point)
    (define-key map [(meta ?\?)]    'maplev-view-at-point)
    (define-key map [(control h) (meta d)] 'maplev-what-proc)

    (define-key map [(control shift mouse-2)] 'maplev-help-follow-mouse)
    (define-key map [(meta shift mouse-2)]    'maplev-view-follow-mouse)

    (define-key map [(control c) (control s) ?h] 'maplev-switch-buffer-help)
    (define-key map [(control c) (control s) ?l] 'maplev-switch-buffer-proc)
    (define-key map [(control c) (control s) ?c] 'maplev-switch-buffer-cmaple)
    map)
  "Keymap used in Maple mode.")

;;}}}
;;{{{ Menu

(defvar maplev-menu nil)
(unless maplev-menu
  (easy-menu-define
    maplev-menu maplev-mode-map
    "Menu for MapleV mode."
    `("MapleV"
      ("Indent"
       ["Buffer"    maplev-indent-buffer t]
       ["Procedure" maplev-indent-procedure t]
       ["Region"    maplev-indent-region t])
      ("Mint"
       ["Buffer"    maplev-mint-buffer t]
       ["Procedure" maplev-mint-procedure t]
       ["Region"    maplev-mint-region t]
       ["Rerun"     maplev-mint-rerun :active maplev-mint--code-beginning])
      ("Maple"
       ["Goto buffer"    maplev-cmaple-pop-to-buffer t]
       ["Send buffer"    maplev-cmaple-send-buffer t]
       ["Send procedure" maplev-cmaple-send-procedure t]
       ["Send region"    maplev-cmaple-send-region t]
       ["Send line"      maplev-cmaple-send-line t]
       "---"
       ["Interrupt"   maplev-cmaple-interrupt t]
       ["Kill"        maplev-cmaple-kill t])
      ("Help"
       ["Word"        maplev-help-at-point t]
       ["Highlighted" maplev-help-region t])
      "---"
      ("Setup"
       ["Enable auto-fill comments" (setq maplev-auto-fill-comment-flag (not maplev-auto-fill-comment-flag))
	:style toggle :selected maplev-auto-fill-comment-flag]
       ["Enable auto-string break" (setq maplev-auto-break-strings-flag (not maplev-auto-break-strings-flag))
	:style toggle :selected maplev-auto-break-strings-flag]
       ["Use leading commas" (setq maplev-leading-comma-flag (not maplev-leading-comma-flag))
	:style toggle :selected maplev-leading-comma-flag]
       ("Decoration"
	["reserved words"  (maplev-reset-font-lock 1) :style radio
	 :selected (equal font-lock-maximum-decoration 1)]
	["+ special words"  (maplev-reset-font-lock 2) :style radio
	 :selected (equal font-lock-maximum-decoration 2)]
	["+ builtin functions"  (maplev-reset-font-lock 3) :style radio
	 :selected (or (equal font-lock-maximum-decoration 3)
		       (equal font-lock-maximum-decoration t))]))
      "---"
      ["Add Index" maplev-add-imenu (not (and (boundp 'imenu--index-alist)
                                              imenu--index-alist))]

      "---"
      ["Quit"      quit-window t]
      "---"
      ["Info"  maplev-goto-info-node t]
      ["Info in Browser" maplev-browse-info]
      ["About" maplev-about t])))

;;}}}
;;{{{ Imenu support

;; Index all the procedure assignments.  Other possiblities to index
;; are global variable assignments, macros and aliases; however,
;; selecting them is difficult.

(defvar maplev-imenu-generic-expression
  `(("Procedures" ,maplev--defun-begin-re 2)
    ("Modules" ,(concat "^\\(" maplev--name-re "\\)"
			"[ \t\n]*:=[ \t\n]*"
			"module") 1)
    ("Macros" ,(concat "^macro([ \t]*\\([^ \t=]*\\)") 1))
  "Imenu expression for MapleV mode.  See `imenu-generic-expression'.")

(defun maplev--imenu-goto-function (name position &rest ignore)
  "Move point to POSITION.  Ignore NAME and IGNORE.
This works with the function `folding-mode', but crudely.
Folding mode appears to have an error; `folding-goto-char' does
not work reliably.  Until that is fixed the solution is to open
the entire buffer."
  (and (or (< position (point-min))
           (> position (point-max)))
       (widen))
  (if folding-mode (folding-open-buffer))
  (goto-char position))

(defun maplev-add-imenu ()
  "Add an imenu of Maple procedures."
  (interactive)
  (imenu-add-to-menubar "Index")
  (menu-bar-mode 1))

(defun maplev--imenu-create-index-function ()
  "Create an index for `imenu'.
Check whether command `folding-mode' is active."
  (if folding-mode (folding-open-buffer))
  (imenu-default-create-index-function))

;;}}}
;;{{{ Buffer edit functions

;; Does this work with folding-mode?
(defun maplev-remove-trailing-spaces  ()
  "Remove trailing spaces in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" nil t)
          (replace-match "" nil nil))))))


(defun maplev-goto-comment ()
  "Move point just after comment character in line.
If there is no comment character in the line, move point to end of line
and return nil, otherwise return t."
  (interactive)
  (beginning-of-line)
  (maplev-indent-validate-info)
  (let ((state  (parse-partial-sexp
                 (maplev-indent-info-point)
                 (point)
                 nil nil (maplev-indent-info-state))))
    (nth 4 (parse-partial-sexp
            (point)
            (line-end-position)
            nil nil state 'comment-stop))))

(defun maplev-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handles Maple comments.
Assigned to `fill-paragraph-function'.  If any of the current line is
a comment, fill the comment or the paragraph of it that point is in,
preserving the comment's indentation and initial comment symbol.
Prefix JUSTIFY means justify as well."
  (interactive "*P")
  (let (has-code      ; Non-nil if line contains code (possibly blank)
        comment-fill-prefix)  ; Appropriate fill-prefix for a comment.

    ;; Figure out what kind of comment we are looking at.

    (save-excursion
      (beginning-of-line)
      (setq has-code (looking-at "[ \t]*[^ \t#]"))
      (when (maplev-goto-comment)
        (backward-char)
        (looking-at "#+[\t ]*")
        (setq comment-fill-prefix
              (concat (if indent-tabs-mode
                          (concat
                           (make-string (/ (current-column) tab-width) ?\t)
                           (make-string (% (current-column) tab-width) ?\ ))
                        (make-string (current-column) ?\ ))
                      (buffer-substring (match-beginning 0) (match-end 0))))
        (save-restriction
          (beginning-of-line)
          (narrow-to-region
           ;; Find the first line we should include in the region to fill.
           (save-excursion
             (while (and (zerop (forward-line -1))
                         (looking-at "^[ \t]*#")))
             ;; We may have gone too far.  Go forward again if there
             ;; is no comment on this line.
             (or (looking-at ".*#")
                 (forward-line 1))
             (point))
           ;; Find the beginning of the first line past the region to fill.
           (save-excursion
             (while (progn (forward-line 1)
                           (looking-at "^[ \t]*#")))
             (point)))

          ;; Lines with only comment characters on them
          ;; can be paragraph boundaries.
          (let* ((paragraph-start    (concat paragraph-start "\\|[ \t#]*$"))
                 (paragraph-separate (concat paragraph-start "\\|[ \t#]*$"))
                 (paragraph-ignore-fill-prefix nil)
                 (fill-prefix comment-fill-prefix)
                 (after-line (if has-code
                                 (save-excursion
                                   (forward-line 1) (point))))
                 (end (progn
                        (forward-paragraph)
                        (or (bolp) (newline 1))
                        (point)))
                 ;; If this comment starts on a line with code,
                 ;; include that line in the filling.

                 (beg (progn (backward-paragraph)
                             (if (eq (point) after-line)
                                 (forward-line -1))
                             (point))))
            (fill-region-as-paragraph beg end
                                      justify nil
                                      (save-excursion
                                        (goto-char beg)
                                        (if (looking-at fill-prefix)
                                            nil
                                          (re-search-forward comment-start-skip)
                                          (point)))))))
      t))) ; return non-nil so fill-paragraph knows this succeeded

;;}}}
;;{{{ Info

;; This must go elsewhere (in maplev-mode).

(defun maplev-goto-info-node ()
  "Go to the info node for maplev."
  (interactive)
  (require 'info)
  (info "maplev"))

(defun maplev-browse-info ()
  "Open the html version of the maplev documentation in a browser."
  (interactive)
  (let ((home (getenv "HOME")))
    (when home
      (let ((html (concat (file-name-as-directory home)
			  "maple/toolbox/maplev/info/maplev.html")))
	(if (file-exists-p html)
	    (browse-url (concat "file://" html))
	  (error "html file not found: %s" html))))))

;;}}}

;;{{{ MapleV mode

;;;###autoload
(define-derived-mode maplev-mode fundamental-mode "MapleV"
  "Major mode for editing Maple code.

\\[maplev-electric-tab] indents the current line.
\\[maplev-indent-newline] indents the current line and inserts a new indented line.
\\[maplev-newline-and-comment] inserts a newline and begins a flush left comment.

\\[maplev-insert-assignment-operator] inserts `:=' with spaces at end of line.
\\[maplev-template-proc] inserts a procedure template after querying for options.
\\[maplev-template-module] inserts a module template after querying for options.
\\[maplev-template-use-statement] inserts a use statement after querying for the expression sequence.

There are functions and keys for indenting code, syntax checking \(via mint\),
displaying Maple help pages and printing the source code of procedures from the
Maple libraries.

Key bindings:
\\{maplev-mode-map}"
  :group 'maplev
  :abbrev-table nil
  :syntax-table maplev-mode-syntax-table

  ;; paragraph filling
  ;;
  ;; The assignment to `paragraph-start' is copied from emacs-lisp.el.
  ;; Note that because `page-delimiter' is, by default, "^\f", that
  ;; is, `^L' anchored to the beginning of the line, the assignment to
  ;; `paragraph-start' violates the explicit warning in the docstring
  ;; about not anchoring this value.  Not a big deal.

  (set (make-local-variable 'paragraph-start)         (concat page-delimiter "\\|$"))
  (set (make-local-variable 'paragraph-separate)       paragraph-start)
  (set (make-local-variable 'fill-paragraph-function) 'maplev-fill-paragraph)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'adaptive-fill-mode)       nil)
  (set (make-local-variable 'auto-fill-inhibit-regexp) (concat "[ \t]*[^  \t#]"))

  (set (make-local-variable 'beginning-of-defun-function) #'maplev-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)       #'maplev-end-of-defun)

  (set (make-local-variable 'require-final-newline) t)

  (auto-fill-mode (if maplev-auto-fill-comment-flag 1 0))
  (setq auto-fill-function 'maplev-auto-fill)

  ;; indentation
  (setq indent-tabs-mode maplev-indent-tabs-mode)

  (set (make-local-variable 'indent-line-function)   #'maplev-indent-line)
  (set (make-local-variable 'indent-region-function) #'maplev-indent-region)
  (set (make-local-variable 'tab-width)               maplev-indent-level)
  (set (make-local-variable 'maplev-indent-declaration) maplev-indent-declaration-level)

  (ad-activate 'fixup-whitespace)

  ;; comments
  (set (make-local-variable 'comment-start)            maplev-comment-start)
  ;;  (if (< emacs-major-version 22)
  ;;      (set (make-local-variable 'block-comment-start)      maplev-block-comment-start))
  (set (make-local-variable 'comment-end)              "")
  (set (make-local-variable 'comment-start-skip)       "#+[ \t]*")
  (set (make-local-variable 'comment-column)           maplev-comment-column)
  (set (make-local-variable 'comment-indent-function) 'maplev-indent-comment-indentation)

  (maplev-set-tab-width)

  ;; menubar (for Xemacs, GNU Emacs doesn't need this)
  ;; (and maplev-menu (easy-menu-add maplev-menu))

  ;; imenu
  (set (make-local-variable 'imenu-create-index-function)
       #'maplev--imenu-create-index-function)
  (set (make-local-variable 'imenu-default-goto-function)
       'maplev--imenu-goto-function)
  (set (make-local-variable 'imenu-generic-expression)
       maplev-imenu-generic-expression)
  (set (make-local-variable 'imenu-case-fold-search) nil)

  ;; aligning rules

  (when (featurep 'align)
    (setq align-mode-rules-list maplev-align-rules-list)
    (setq align-mode-exclude-rules-list maplev-align-exclude-rules-list))

  ;; Font lock support: make these variables buffer-local
  ;; so that we can change the decoration level
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-maximum-decoration)

  ;; Mint support
  (make-local-variable 'maplev-mint--code-beginning)
  (make-local-variable 'maplev-mint--code-end)

  ;; Is this what one wants??
  ;; (set (make-local-variable 'beginning-of-defun-function) #'(lambda () (maplev-view-beginning 1 t)))
  ;; (set (make-local-variable 'end-of-defun-function)       #'(lambda () (maplev-view-end 1 t)))
  ;; (set (make-local-variable 'add-log-current-defun-function)
  ;;      #'maplev-current-defun-name) ;; not yet available

  (maplev-add-maple-to-compilation)

  (maplev-reset-font-lock)

  (when maplev-buttonize-includes-flag
    (maplev-buttonize-includes)
    (maplev-buttonize-links))

  ;; Create configuration object
  (if maplev-load-config-file-flag (maplev-load-config-file))
  (unless maplev-config
    (maplev-config))

  ;; Set hooks
  (if maplev-clean-buffer-before-saving-flag
      (add-hook 'write-file-functions 'maplev-remove-trailing-spaces))
  ;;(make-local-hook 'before-change-functions)
  (add-hook 'before-change-functions 'maplev-indent-before-change-function nil t)
  (run-hooks 'maplev-mode-hook))

;;}}}

;;{{{ Electric functions

(defun maplev-indent-newline ()
  "Indent current line, insert a newline and indent the new line.
Current line is not indented if it is a comment.  Remove trailing
whitespace."
  (interactive "*")
  (or (maplev--comment-line-indentation) ; nil if a comment
      (maplev-indent-line))
  (delete-horizontal-space)             ; remove trailing whitespace
  (newline)
  (maplev-indent-line))

(defun maplev-insert-assignment-operator ()
  "Insert the Maple assignment operator after last nonwhite character."
  (interactive "*")
  (end-of-line)
  (skip-chars-backward " \t")
  (delete-region (point) (line-end-position))
  (insert maplev-assignment-operator))

(defun maplev-electric-tab ()
  "Indent the current line."
  (interactive "*")
  (maplev-indent-line))

(defun maplev-newline-and-comment ()
  "Insert a newline and start a new comment line.
If the current line is a code line, the comment is set flush left,
otherwise it is aligned with the previous code line."
  (interactive "*")
  (newline)                             ; should we indent?
  (let ((indent (maplev--comment-line-indentation -1)))
    (and indent (indent-to indent)))
  (insert maplev-block-comment-start)
  )


(defun maplev--comment-line-indentation (&optional n)
  "Return the indentation of a Maple comment line, nil if not a comment line.
Optionally move N lines forward before testing.  Point is not affected."
  (save-excursion
    (forward-line (or n 0))
    (and (looking-at "^[ \t]*#") (current-indentation))))

(defun maplev-untab ()
  "Delete backwards to previous tab stop."
  (interactive "*")
  (backward-delete-char-untabify
   (let ((ind (% (current-column) maplev-indent-level)))
     (and (= ind 0) (setq ind maplev-indent-level))
     (if (> ind (current-column))
         (current-column)
       ind))))

;;}}}

;;{{{ Folding functions

(with-no-warnings
  (defun maplev-fold-proc ()
    "Add editor fold marks around the procedure at point.
The name of the procedure is inserted into the title of the fold."
    (interactive)
    (let ((proc (maplev-what-proc 'nodisplay)))
      (maplev-mark-defun)
      (folding-fold-region (point) (mark))
      (insert (concat " " proc))
      (folding-shift-out))))

;;}}}

;;{{{ Movement functions

(defconst maplev--operator-re
  (concat "\\(?:"
	  "\\<"
	  (regexp-opt '("and" "assuming" "implies" "in" "intersect"
			"minus" "mod" "not" "or" "subset" "union" "xor"))
	  "\\>"
	  "\\|"
          (regexp-opt
           '(":-"
             "||"
             "::"
             "!"
             "^" "@@"
             "." "*" "&*" "/" "@"
             "+" "-"
             ".."
             "<" "<=" ">" ">=" "=" "<>"
             "$"
             "->"
             ;; ","
             ":="
             ))
          ;; neutral operators
          "\\|&\\(?:[~!@$^*-+=\"<>,./?]+\\|[a-zA-Z_][a-zA-Z_0-9]*\\)"
          "\\)")
  "Regular expression matching a Maple operator."
  )

(defconst maplev--number-re
  "[+-]?\\(?:[0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\(?:[Ee][+-]?[0-9]*\\)?"
  "Regular expression matching a number.
This is slightly too aggressive, it incorrectly matches, d.Ed, which is invalid.")

(defconst maplev--expr-re
  (concat "\\s-*"
          "\\(?:"
          maplev--operator-re
          "\\|"
          maplev--number-re
          "\\|"
          maplev--symbol-re
          "\\|"
          maplev--string-re
          "\\)"
          "\\s-*"
          )
  "Regular expression to match a partial expression.")

(defun maplev-forward-expr ()
  "Move point forward over a complete expression.
This is a hack and is hardly robust."
  (interactive)
  (if (cond
       ((looking-at "\\s-*\\s(")
	(forward-sexp)
	t)
       ((looking-at maplev--expr-re)
	(goto-char (match-end 0)))
       ((looking-at "\\s-*\\(?:#.*\\)?$") ; inline-comment
	(forward-line)
	(not (eobp))))
      (maplev-forward-expr)))

(defconst maplev-wexp-statement-start-re
  (concat "\\<"
	  (mapconcat 'identity (list
				(regexp-opt '("proc" "module" "do" "if" "use" "try") t)
				(regexp-opt '("for" "from" "to" "while") t))
		     "\\|")
	  "\\>")
  "Regular expression matching a Maple keyword that starts a statemnt.")

(defconst maplev-wexp-statement-cont-re
  (concat "\\<"
	  (mapconcat 'identity (list
				(regexp-opt '("proc" "module" "do" "if" "use" "try") t)
				(regexp-opt '("fi" "od") t)
				(regexp-opt '("end") t))
		     "\\|")
	  "\\>")
  "Regular expression matching a Maple keyword that continues or ends a statement.")

(defun maplev-forward-wexp ()
  "Move forward over a well-formed Maple expression."
  (interactive)
  ;; assume for now point is not in string/comment
  ;; move forward over white space
  (re-search-forward "\\=\\(?:[ \t]+\\|\n+\\)+" nil t)
  (cond
   ((looking-at maplev-wexp-statement-start-re)
    ;; move to end of statement
    (let ((cnt (if (match-string 1) 1 0))
	  keyword)
      (goto-char (match-end 0))
      (while (progn
	       (maplev--re-search-forward maplev-wexp-statement-cont-re)
	       (setq cnt (+ cnt (if (match-string 1) 1 -1)))
	       (when (and (match-string 3) ;; matched "end"
			  (looking-at "\\s-+\\(?:do\\|if\\|module\\|proc\\|try\\|use\\)\\>"))
		 (goto-char (match-end 0)))
	       (not (zerop cnt)))))
    ;; move past whitespace
    (re-search-forward "\\=\\(?:[ \t]+\\|\n+\\)+" nil t)
    ;; may need to move over arguments: proc() ... end proc (x,y,z)
    (when (looking-at ";\\|:[^:]")
      (goto-char (match-end 0))))))

;;}}}

;;{{{ Miscellaneous

(defun maplev-remove-dupes (list)
  "Remove duplicates from a sorted assoc LIST."
  (let (tmp-list head)
    (while list
      (setq head (pop list))
      (unless (equal (car head) (car (car list)))
        (push head tmp-list)))
    (reverse tmp-list)))

(defun maplev-minus (setA setB)
  "Remove members of SETB from SETA."
  (let (tmp head)
    (while setA
      (setq head (pop setA))
      (unless (member head setB)
	(push head tmp)))
    (reverse tmp)))

;;}}}

;;{{{ Templates

(defun maplev--template-proc-module (function name args description)
  "Insert a template for a Maple FUNCTION \(\"proc\" or \"module\"\).
Use NAME, ARGS, and DESCRIPTION.  Move point to body of FUNCTION.

If NAME equals \"\" then the function is anonymous,
no assignment operator is inserted and the closing
end statement is not terminated with a colon.

ARGS are inserted as formal arguments in the function statement.

If `maplev-insert-copyright-flag' is non-nil, then insert a copyright
as an option statement.  Confirmation is required for an anonymous
function.

Unless DESCRIPTION equals \"\" it is inserted as a description statement.

If `maplev-comment-end-flag' is non-nil, and the function is not
anonymous, then NAME is inserted as a comment following the closing
end statement.  Point is moved to the start of the function body."
  (let ((fname (not (string= name ""))))
    ;; Insert assignment if function has a name
    (when fname
      (setq name (maplev--string-to-name name))
      (insert name " := "))
    (insert function
            (make-string maplev-variable-spacing ?\ )
            "(" args ")")          ; Insert function, with formal args

    ;; Copyright notice
    (when (and maplev-insert-copyright-flag
               (or fname (y-or-n-p "Insert copyright? ")))
      (insert "\noption `Copyright (C) "
              (format-time-string "%Y" (current-time))
              " by " maplev-copyright-owner ". All rights reserved.`;"))

    ;; description
    (unless (string= description "")
      (insert "\ndescription " maplev-description-quote-char
              description maplev-description-quote-char ";"))

    (insert "\n\nend " function)
    (when fname
      (insert ":")
      (if maplev-comment-end-flag
          (insert maplev-template-end-comment name)))
    (forward-line -1)                   ; Move point to start of body
    ;; bug in maplev-current-defun:
    ;; it doesn't work yet with anonymous procedures
    (when fname (maplev-indent-procedure))))

(defun maplev-template-proc (name args description)
  "Insert a template for a Maple procedure and move point to its body.
Prompt for the NAME, ARGS, and DESCRIPTION.  See `maplev-template'."
  (interactive "*sName (return for anonymous) \nsArguments: \nsDescription: ")
  (maplev--template-proc-module "proc" name args description))

(defun maplev-template-module (name args description)
  "Insert a template for a Maple module and move point to its body.
Prompt for the NAME, ARGS, and DESCRIPTION.  See `maplev-template'."
  (interactive "*sName (return for anonymous) \nsArguments: \nsDescription: ")
  (maplev--template-proc-module "module" name args description))

(defun maplev-template-use-statement (exprseq)
  "Insert a template for a Maple use statement and move point to its first statement.
Prompt for the EXPRSEQ."
  (interactive "*sExpression Sequence: ")
  (insert "use " exprseq " in")
  (maplev-indent-newline)
  (insert "\nend use")
  (maplev-indent-line)
  (forward-line -1)
  (maplev-indent-line))

;;}}}
;;{{{ Completion

;; Define functions for completing Maple symbols.
;;
;; It is easy enough to collect all the symbols defined in
;; ?index/functions and ?index/packages.  However, we would really
;; like to complete on the exports of particular Maple modules.  It is
;; not practical, nor useful, to complete on all exports of all
;; modules, not is it straightforward to provide intelligent
;; completion, that is, inside a `use <module>' statement complete on
;; the exports of <module>.  A reasonable workaround is to provide a
;; function that allows the user to add the exports of selected
;; modules to the completion list.

(defun maplev-add-exports-of-module-at-point (module)
  "Add the exports of MODULE at point to `maplev-completions'.
The real work is done by `maplev-complete-on-module-exports'."
  (interactive (list (maplev-ident-around-point-interactive
                      "Complete on Maple exports of module")))
  (maplev-complete-on-module-exports module))

(defun maplev-complete-on-module-exports (module)
  "Add the exports of MODULE to `maplev-completions'."

  (with-current-buffer (maplev--cmaple-buffer)
    (save-restriction
      ;; Print each export of module on a separate line in a narrowed buffer.
      (narrow-to-region (point-max) (point-max))
      (maplev-cmaple--send-string
       (maplev--cmaple-process)
       (concat "seq(lprint(e),e=exports(" module "));"))
      ;; Delete the input line.
      (delete-region
       (goto-char (point-min))
       (progn (forward-line) (point)))
      ;; Check that no Maple error occurred.
      ;; If so, assume that module is not an actual Maple module
      ;; and print a temporary message at the bottom of the screen.
      (if (looking-at "Error")
          (progn
            (ding)
            (message "The argument `%s' is not a Maple module" module)
            (sit-for 2))
        ;; Initialize completions to those previously assigned
        (let ((completions maplev-completions))
          ;; Goto end of buffer and read upwards, a line at a time,
          ;; adding it to the exports list.
          (goto-char (point-max))
          (while (zerop (forward-line -1))
            (setq completions
                  (cons (cons (buffer-substring-no-properties
                               (point) (line-end-position))
                              nil)
                        completions)))
          ;; Replace the completion list.
          (setq maplev-completions
		(maplev-remove-dupes
		 (sort completions #'(lambda (a b) (string< (car a) (car b))))))))
      ;; Delete the output from the cmaple buffer.
      (delete-region (point-min) (point-max)))))

(defun maplev--generate-initial-completions ()
  "Generate `maplev-completions' from maple help pages.
If it already exists, do nothing."
  (unless maplev-completions)

  ;; To make it easy to pick out the package names from the
  ;; index/package help page, set the interface variable
  ;; `screenwidth' to infinity and save the original value in the
  ;; elisp variable screenwidth.
  
  (let ((screenwidth (maplev-cmaple-direct
		      "lprint(interface('screenwidth'=infinity));" t))
	completions)
    (unwind-protect
	(with-current-buffer (get-buffer-create (maplev--help-buffer))
	  ;; Process help node "index/function".
	  ;; (while (maplev-cmaple--locked-p) (maplev--short-delay))
	  (maplev-help-show-topic "index/function" 'hide)
	  ;; (while (maplev-cmaple--locked-p) (maplev--short-delay))
	  (save-restriction
	    (narrow-to-region
	     (re-search-forward "^    ")
	     (save-excursion (goto-char (point-max))
			     (search-backward "See Also")))
	    (goto-char (point-max))
	    (while (backward-word)
	      (setq completions
		    (cons (cons (buffer-substring-no-properties
				 (point)
				 (save-excursion (forward-word) (point)))
				nil)
			  completions))))

	  ;; Process help node "index/package".
	  ;; (while (maplev-cmaple--locked-p) (maplev--short-delay))
	  (maplev-help-show-topic "index/package" 'hide)
	  ;; (while (maplev-cmaple--locked-p) (maplev--short-delay))
	  (save-restriction
	    (narrow-to-region
	     (progn (re-search-forward "^    \\w" nil t)
		    (goto-char (match-beginning 0))) ; first package
	     (progn (re-search-forward "^-" nil t)
		    (goto-char (match-beginning 0)))) ; bullets after packages
	    (goto-char (point-max))
	    ;; Assign a regular expression to match each package name;
	    ;; the name is matched by the first group in regexp.
	    (let ((regexp (concat
			   "^\\s-+"   ; whitespace at start of line
			   "\\(" maplev--name-re "\\)"))) ; package name (first group)
	      (while (re-search-backward regexp nil 'move)
		(setq completions
		      (cons (cons (buffer-substring-no-properties
				   (match-beginning 1) (match-end 1))
				  nil)
			    completions)))))
	  ;; Delete both help pages.
	  (maplev-history-delete-item)
	  ;; (while (maplev-cmaple--locked-p) (maplev--short-delay))
	  (maplev-history-delete-item))

      ;; Assign `maplev-completions'.  Sort the completions.
      (setq maplev-completions (sort completions #'(lambda (a b) (string< (car a) (car b)))))
      ;; Restore the original interface screenwidth.
      (maplev-cmaple-direct (concat "interface('screenwidth'=" screenwidth ");") t))))


(defun maplev--completion (word predicate mode)
  "Generate minibuffer completion using maple function names.
For the meaning of args see Info node `(elisp)Programmed Completion'."
  (maplev--generate-initial-completions)
  (let ((possibilities maplev-completions))
    (cond ((eq mode t)
	   (all-completions word possibilities predicate))
	  ((not mode)
	   (try-completion word possibilities predicate))
	  ((eq mode 'lambda)
	   (assoc word possibilities)))))

(defun maplev-complete-symbol (&optional prefix)
  "Perform completion on maple symbol preceding point.
Compare that symbol against `maplev-completions'."
  ;; Code borrowed from lisp-complete-symbol.
  (interactive)
  (let* ((end (point))
	 (beg (save-excursion
                (backward-sexp 1)
                (point)))
	 (pattern (buffer-substring-no-properties beg end))
	 (completion (try-completion pattern 'maplev--completion)))
    (cond ((eq completion t))
	  ((null completion)
	   (message "Cannot find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (message "Making completion list...")
	   (let ((list (sort (all-completions pattern 'maplev--completion)
                             'string<)))
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list list)))
	   (message "Making completion list...%s" "done")))))

;;}}}

;;{{{ Comments to Strings

(defun maplev-comment-to-string-region (beg end)
  "Convert indented comments to strings in region from BEG to END.
The purpose of this is to embed comments as strings into the source
so that, when using a debugger, the showstat output appears to
be commented.  See `maplev-string-to-comment-region'."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(\\s-+\\)\\(#.*\\)" end t)
      (replace-match "\\1\"\\2\";"))))

(defun maplev-string-to-comment-region (beg end)
  "Convert strings back to comments in region from BEG to END.
This is the inverse of `maplev-comment-to-string-region.'"
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(\\s-+\\)\"\\(#.*\\)\";$" end t)
      (replace-match "\\1\\2"))))

;;}}}

;;{{{ Auto-fill

(defun maplev-auto-fill ()
  "Use this function when `auto-fill-mode' is active in Maple.
If `maplev-auto-break-strings-flag' is non-nil, a string that exceeds
the current column is automatically broken at whitespace, terminated
with a double-colon, and begun again on the next line, with an indent."
  (let ((fc (current-fill-column)))
    (and fc (<= fc (current-column))
	 (if (and
	      maplev-auto-break-strings-flag
	      (eq ?\" (nth 3 (parse-partial-sexp (line-beginning-position) (point)))))
	     (maplev-auto-break-string)
	   (do-auto-fill)))))

(defun maplev-auto-break-string ()
  "Auto-break a string.  Must be called where the string is to break.
Inserts double-quote, then calls `mpldoc-indent-newline' to insert an
indented newline.  A double-quote is inserted at the indentation point.
If at the end of the line, a closing double-quote is also added and point
moved to be before it."
  (insert "\"")
  (newline-and-indent)
  (insert-char ?\" 1)
  (when (eolp)
    (insert-char ?\" 1)
    (backward-char)))



;;}}}

;;{{{ files

(defun maplev-expand->file-name (>file mroot)
  "If the string >FILE starts with >, replace it with MROOT."
  (if (= (aref >file 0) ?>)
      (concat (file-name-as-directory mroot)
	      (substring >file 1))
    >file))

;;}}}
;;{{{ tab-width

(defvar maplev-get-tab-width-function nil
  "Use this to modify the tab width used by maplev on a per-file basis.
If assigned it is passed the name of the file and should return
the desired tab width.")

(defun maplev-set-tab-width (&optional file)
  "Return the value of tab width required by optional FILE, or if nil,
the file name given be `buffer-file-name'.  If the function
`maplev-get-tab-width-function' is assigned, call it with FILE,
otherwise use `maplev-tab-width'."
  (setq tab-width (if (functionp 'maplev-get-tab-width-function)
		      (funcall maplev-get-tab-width-function (or file (buffer-file-name)))
		    maplev-tab-width)))

;;}}}

;;{{{ Font lock

(defvar maplev-preprocessor-face 'maplev-preprocessor-face
  "*Face name for Maple preprocessor directives.")

(defface maplev-preprocessor-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))  (:foreground "DimGray"   :bold t))
    (((class color)     (background light)) (:foreground "dark orange"))
    (((class color)     (background dark))  (:foreground "orange"))
    (t (:bold t)))
  "Font lock mode face used for Maple preprocessor directives."
  :group 'maplev-faces)


;;{{{   reserved words

(defconst maplev-reserved-words
  '("and" "assuming" "break" "by" "catch"
    "description"  "do" "done" "elif" "else"
    "end" "error" "export" "fi" "finally"
    "for" "from" "global" "if" "implies"
    "in" "intersect" "local" "minus" "mod"
    "module" "next" "not" "od" "option"
    "options" "or" "proc" "quit" "read"
    "return" "save" "stop" "subset" "then"
    "to" "try" "union" "until" "use" "uses"
    "while" "xor")
  "List of reserved words in Maple.")

(defconst maplev-special-words
  '("args" "nargs" "procname" "RootOf" "thismodule" "thisproc"
    "_options" "_noptions" "_rest" "_nrest"
    "_params" "_nparams" "_passed" "_npassed"
    "_nresults" "static")
  "List of special words in Maple.")

(defconst maplev-initial-variables
  '("Catalan" "true" "false" "FAIL" "infinity" "Pi" "gamma"
    "integrate" "libname" "NULL" "Order" "printlevel" "lasterror" "lastexception"
    "Digits" "constants" "undefined" "I"
    "UseHardwareFloats"
    "Testzero" "Normalizer" "NumericEventHandlers"
    "Rounding" ;; "`mod`" "`index/newtable`"
    )
  "List of initial variables in Maple.")

;;}}}

(defconst maplev--deprecated-re
  (eval-when-compile
    (maplev--list-to-word-re
     (list "queue" "stack" "traperror" "linalg" "solvefor" "ERROR")))
  "Regex of deprecated keywords and procedures.")

(defconst maplev--special-words-re
  (maplev--list-to-word-re maplev-special-words)
  "Regex of special words in Maple.")

(defconst maplev--initial-variables-re
  (maplev--list-to-word-re maplev-initial-variables)
  "Regexp of global, environmental variables and constants.")

(defconst maplev--preprocessor-directives-re
  (eval-when-compile
    (concat "^\\$\\("
	    (regexp-opt (list
			 "define" "elif" "elifdef" "elifndef" "else" "endif"
			 "file" "ifdef" "ifndef" "include" "undef"
			 ))
	    "\\)"))
  "Regex of preprocessor directives.")

(defconst maplev--include-directive-re
  "^\\(?:## \\)?\\$include\\s-*\\([<\"]\\)\\(.*\\)[>\"]"
  "Regex of an include directive.
The first group matches the character used to delimit the
file (either < or \").  The second group matches the filename.")

(defconst maplev-constructors
  (list "Complex" "Float" "Fraction" "HFloat" "Integer" "SFloat")
  "List of Maple constructors.")

;;{{{  builtins

;; Currently the backquoted builtin functions are font-locked as
;; quoted names rather than as builtin functions.  Fixing this
;; requires pulling them out.

(defconst maplev-builtin-types
  '(;"`::`" "`..`" "`!`"
    "algebraic" "anyfunc" "anything" "atomic"
    "boolean"
    "complex" "constant" "cx_infinity" "cx_zero"
    "embedded_axis" "embedded_imaginary" "embedded_real"
    "equation" "even" "extended_numeric" "extended_rational"
    "finite" "float" "fraction" "function"
    "identical" "imaginary" "indexable" "indexed" "integer"
    "list" "literal"
    "module" "moduledefinition"
    "name" "neg_infinity" "negative" "negint" "negzero"
    "nonnegative" "nonnegint" "nonposint" "nonpositive"
    "nonreal" "numeric" "odd"
    "polynom" "pos_infinity" "posint" "positive" "poszero"
    "procedure" "protected"
    "radical" "range" "rational" "ratpoly" "real_infinity"
    "realcons" "relation"
    "sequential" "set" "sfloat" "specfunc" "string" "symbol"
    "tabular" "uneval" "zppoly")
  "List of builtin Maple types.")

(defconst maplev-builtin-functions
  ;; use sort([anames('builtin')]) to generate
  '(;; "`$`" "`*`" "`**`" "`+`" "`..`" "`::`" "`<`" "`<=`" "`<>`" "`=`" "`>`" "`>=`" "`?()`" "`?[]`" "`[]`" "`^`"
    ;; "`and`" "`if`" "`evalf/hypergeom/kernel`" "`int/series`" "`intersect`" "`kernel/transpose`" "`not`"
    ;; "`or`" "`quit`" "`union`" "`xor`" "`{}`" "`||`" "`~`"
    ;; "`and`" "and=`"
    "ASSERT" "Array" "ArrayOptions" "CopySign"
    "DEBUG" "Default0" "DefaultOverflow" "DefaultUnderflow" "ERROR"
    "EqualEntries" "EqualStructure" "FromInert" "Im" "MPFloat"
    "MorrBrilCull" "NameSpace" "NextAfter" "Normalizer" "NumericClass"
    "NumericEvent" "NumericEventHandler" "NumericStatus" "Object"
    "OrderedNE" "RETURN" "Re" "Record" "SDMPolynom" "SFloatExponent"
    "SFloatMantissa" "Scale10" "Scale2" "SearchText" "String" "TRACE" "ToInert"
    "Unordered" "UpdateSource" "_error" "_hackwareToPointer" "_jvm"
    "_local" "_maplet" "_mint" "_savelib" "_treeMatch" "_unify" "_xml"
    "abs" "add" "addressof" "anames" "`and`" "`and=`" "andmap" "andseq" "appendto" "array"
    "assemble" "assign" "assigned" "attributes" "bind" "call_external"
    "callback" "cat" "coeff" "coeffs" "conjugate" "convert" "crinterp"
    "debugopts" "define_external" "degree" "denom" "diff" "disassemble"
    "divide" "dlclose" "`done`" "entries" "eval" "evalb" "evalf" "`evalf/hypergeom/kernel`"
    "evalgf1" "evalhf" "evalindets" "evaln"
    "expand" "exports" "factorial" "frem" "frontend" "gc" "genpoly"
    "gmp_isprime" "goto" "has" "hastype" "hfarray" "icontent" "`if`" "ifelse"
    "igcd" "ilcm" "ilog10" "ilog2" "`implies`" "`implies=`" "indets" "indices" "inner"
    "`int/series`" "`intersect`" "`intersect=`" 
    "iolib" "iquo" "iratrecon" "irem" "is_gmp" "isqrt"
    "`kernel/transpose`" "kernelopts" "lcoeff" "ldegree" "length"
    "lexorder" "lhs" "localGridInterfaceRun" "lowerbound" "lprint"
    "macro" "map" "map2" "max" "maxnorm" "member" "membertype" "min"
    "`minus`" "`minus=`" "`mod`" "`mod=`" "modp" "modp1" "modp2" "mods" "mul" "mvMultiply"
    "negate" "nops" "normal" "`not`" "numboccur" "numelems" "numer"
    "op" "`or`" "`or=" "order" "ormap" "orseq" "overload" "parse" "piecewise" "pointto"
    "print" "print_preprocess" "`quit`" "readlib" "reduce_opr" "remove" "rhs"
    "rtable" "rtableInfo" "rtable_convolution" "rtable_eval" "rtable_histogram"
    "rtable_indfns" "rtable_is_zero" "rtable_normalize_index"
    "rtable_num_dims" "rtable_num_elems" "rtable_options" "rtable_redim" "rtable_rotate"
    "rtable_scale" "rtable_scanblock" "rtable_size" "rtable_sort_indices" "rtable_zip"
    "savelib" "searchtext" "select" "selectremove" "seq"
    "series" "setattribute" "sign" "sort" "ssystem" "`stop`" "streamcall"
    "subs" "`subset`" "subsindets" "subsop" "substring" "system" "table"
    "taylor" "tcoeff" "time" "timelimit" "traperror" "trunc" "type"
    "typematch" "unames" "unbind" "`union`" "`union=`" "upperbound" "userinfo"
    "whattype" "writeto" "`xor`" "`xor=`" "xormap" "xorseq"
    "~Array" "~Matrix" "~Vector")
  "List of builtin functions as of Maple 2022.")

;; (defconst maplev--builtin-functions-alist
;;  '((3 .  ("`$`"                                                                                                                                                                                                                             "ERROR"                                             "Im"                                                                                                                                            "RETURN" "Re"                                                                            "SearchText"                                                                                            "abs"       "addressof" "alias" "anames"                  "appendto" "array" "assemble" "assigned"                                            "callback" "cat" "coeff" "coeffs"             "convert"            "debugopts"                   "degree"         "diff" "disassemble" "divide"                    "entries" "eval" "evalb" "evalf" "`evalf/hypergeom`"                  "evalhf" "evaln" "expand"                              "frontend" "gc" "genpoly"                    "goto" "has" "hastype"           "icontent" "`if`" "igcd" "ilog10"                     "indets" "indices"         "intersect" "`int/series`"         "iquo" "irem"          "isqrt"                                   "lcoeff" "ldegree" "length" "lexorder"       "lprint" "macro" "map"        "max" "maxnorm" "member" "min" "`minus`"         "modp" "modp1"         "mods"                             "nops" "normal"         "numboccur" "numer" "op"        "order"                    "parse"             "pointto" "print" "printf" "protect"          "readlib" "readline"                                                                                                                                                                                                                                                                                                                          "searchtext" "select"                "seq" "series"                                   "sign" "sort" "sscanf" "ssystem"                       "subs"            "subsop" "substring" "system" "table" "taylor" "tcoeff" "time"             "traperror" "trunc" "type"             "unames"          "`union`" "unprotect" "userinfo" "words" "writeto"         ))
;;    (4 .  ("`$`" "`*`"        "`+`"                                                                                                       "ASSERT"                                   "DEBUG"                                                 "ERROR"                                             "Im"           "MorrBrilCull"                                                                                                                   "RETURN" "Re"                                                                            "SearchText"                                                                                            "abs" "add" "addressof" "alias" "anames"                  "appendto" "array" "assemble" "assigned" "attributes"                               "callback" "cat" "coeff" "coeffs"             "convert"            "debugopts"                   "degree" "denom" "diff" "disassemble" "divide"                    "entries" "eval" "evalb" "evalf" "`evalf/hypergeom`"                  "evalhf" "evaln" "expand"                              "frontend" "gc" "genpoly" "getuserinterface" "goto" "has" "hastype"           "icontent" "`if`" "igcd" "ilog10"                     "indets" "indices" "inner" "intersect" "`int/series`" "iolib" "iquo" "irem"          "isqrt" "kernelopts" "`kernel/transpose`" "lcoeff" "ldegree" "length" "lexorder"       "lprint" "macro" "map" "map2" "max" "maxnorm" "member" "min" "`minus`"         "modp" "modp1"         "mods" "mul"                       "nops" "normal"         "numboccur" "numer" "op"        "order"                    "parse"             "pointto" "print"                             "readlib"                                                                                                                                                                                                                                                                                                                                     "searchtext" "select"                "seq" "series" "setattribute" "setuserinterface" "sign" "sort"          "ssystem"                       "subs"            "subsop" "substring" "system" "table" "taylor" "tcoeff" "time"             "traperror" "trunc" "type" "typematch" "unames"          "`union`"             "userinfo"         "writeto"         ))
;;    (5 .  ("`$`" "`*`" "`**`" "`+`"               "`<`" "`<=`" "`<>`" "`=`" "`>`" "`>=`"                                            "`^`" "ASSERT"                                   "DEBUG"                                                 "ERROR"                                             "Im"           "MorrBrilCull"                                                                                                                   "RETURN" "Re"                                                                            "SearchText"                                                                                            "abs" "add" "addressof" "alias" "anames"                  "appendto" "array" "assemble" "assigned" "attributes"        "call"                 "callback" "cat" "coeff" "coeffs"             "convert" "crinterp" "debugopts" "define"          "degree" "denom" "diff" "disassemble" "divide"                    "entries" "eval" "evalb" "evalf" "`evalf/hypergeom/kernel`"           "evalhf" "evaln" "expand"                              "frontend" "gc" "genpoly" "getuserinterface" "goto" "has" "hastype" "hfarray" "icontent" "`if`" "igcd" "ilog10"                     "indets" "indices" "inner" "intersect" "`int/series`" "iolib" "iquo" "irem"          "isqrt" "kernelopts" "`kernel/transpose`" "lcoeff" "ldegree" "length" "lexorder"       "lprint" "macro" "map" "map2" "max" "maxnorm" "member" "min" "`minus`"         "modp" "modp1"         "mods" "mul"                       "nops" "normal"         "numboccur" "numer" "op"        "order"                    "parse"             "pointto" "print"                             "readlib"                                                                                                                                                                                                                                                                                                                                     "searchtext" "select"                "seq" "series" "setattribute" "setuserinterface" "sign" "sort"          "ssystem"                       "subs"            "subsop" "substring" "system" "table" "taylor" "tcoeff" "time" "timelimit" "traperror" "trunc" "type" "typematch" "unames"          "`union`"             "userinfo"         "writeto"         ))
;;    (6 .  ("`$`" "`*`" "`**`" "`+`"               "`<`" "`<=`" "`<>`" "`=`" "`>`" "`>=`"                        "`||`"              "`^`" "ASSERT" "Array" "ArrayOptions" "CopySign" "DEBUG" "Default0" "DefaultOverflow" "DefaultUnderflow" "ERROR" "EqualEntries" "EqualStructure" "FromInert" "Im" "MPFloat" "MorrBrilCull" "NextAfter"              "NumericClass" "NumericEvent" "NumericEventHandler" "NumericStatus"          "OrderedNE" "RETURN" "Re"                       "SFloatExponent" "SFloatMantissa" "Scale10" "Scale2" "SearchText" "TRACE" "ToInert" "Unordered"                                                              "abs" "add" "addressof" "alias" "anames" "`and`"          "appendto" "array" "assemble" "assigned" "attributes" "bind"        "call_external" "callback" "cat" "coeff" "coeffs" "conjugate" "convert" "crinterp" "debugopts" "define_external" "degree" "denom" "diff" "disassemble" "divide"           "`done`" "entries" "eval" "evalb" "evalf" "`evalf/hypergeom/kernel`" "evalgf1" "evalhf" "evaln" "expand" "exports"             "frem" "frontend" "gc" "genpoly"                    "goto" "has" "hastype" "hfarray" "icontent" "`if`" "igcd" "ilog10" "ilog2"             "indets" "indices" "inner" "intersect" "`int/series`" "iolib" "iquo" "irem"          "isqrt" "kernelopts" "`kernel/transpose`" "lcoeff" "ldegree" "length" "lexorder" "lhs" "lprint" "macro" "map" "map2" "max" "maxnorm" "member" "min" "`minus`"         "modp" "modp1" "modp2" "mods" "mul" "mvMultiply" "negate" "nops" "normal" "`not`" "numboccur" "numer" "op" "`or`" "order"                    "parse"             "pointto" "print"                    "`quit`" "readlib"              "remove" "rhs" "rtable" "rtableInfo"                                                       "rtable_indfns" "rtable_is_zero" "rtable_normalize_index" "rtable_num_dims" "rtable_num_elems" "rtable_options"                               "rtable_scanblock"               "rtable_sort_indices"                        "searchtext" "select" "selectremove" "seq" "series" "setattribute"                    "sign" "sort"          "ssystem" "`stop`" "streamcall" "subs"            "subsop" "substring" "system" "table" "taylor" "tcoeff" "time" "timelimit" "traperror" "trunc" "type" "typematch" "unames" "unbind" "`union`"             "userinfo"         "writeto"         ))
;;    (7 .  ("`$`" "`*`" "`**`" "`+`"               "`<`" "`<=`" "`<>`" "`=`" "`>`" "`>=`"                        "`||`"              "`^`" "ASSERT" "Array" "ArrayOptions" "CopySign" "DEBUG" "Default0" "DefaultOverflow" "DefaultUnderflow" "ERROR" "EqualEntries" "EqualStructure" "FromInert" "Im" "MPFloat" "MorrBrilCull" "NextAfter"              "NumericClass" "NumericEvent" "NumericEventHandler" "NumericStatus"          "OrderedNE" "RETURN" "Re"                       "SFloatExponent" "SFloatMantissa" "Scale10" "Scale2" "SearchText" "TRACE" "ToInert" "Unordered"                                 "_treeMatch" "_unify" "_xml" "abs" "add" "addressof" "alias" "anames" "`and`"          "appendto" "array" "assemble" "assigned" "attributes" "bind"        "call_external" "callback" "cat" "coeff" "coeffs" "conjugate" "convert" "crinterp" "debugopts" "define_external" "degree" "denom" "diff" "disassemble" "divide" "dlclose" "`done`" "entries" "eval" "evalb" "evalf" "`evalf/hypergeom/kernel`" "evalgf1" "evalhf" "evaln" "expand" "exports" "factorial" "frem" "frontend" "gc" "genpoly"                    "goto" "has" "hastype" "hfarray" "icontent" "`if`" "igcd" "ilog10" "ilog2" "'implies'" "indets" "indices" "inner" "intersect" "`int/series`" "iolib" "iquo" "irem"          "isqrt" "kernelopts" "`kernel/transpose`" "lcoeff" "ldegree" "length" "lexorder" "lhs" "lprint" "macro" "map" "map2" "max" "maxnorm" "member" "min" "`minus`"         "modp" "modp1" "modp2" "mods" "mul" "mvMultiply" "negate" "nops" "normal" "`not`" "numboccur" "numer" "op" "`or`" "order"                    "parse"             "pointto" "print"                    "`quit`" "readlib"              "remove" "rhs" "rtable" "rtableInfo"                                                       "rtable_indfns" "rtable_is_zero" "rtable_normalize_index" "rtable_num_dims" "rtable_num_elems" "rtable_options"                               "rtable_scanblock"               "rtable_sort_indices"                        "searchtext" "select" "selectremove" "seq" "series" "setattribute"                    "sign" "sort"          "ssystem" "`stop`" "streamcall" "subs" "`subset`" "subsop" "substring" "system" "table" "taylor" "tcoeff" "time" "timelimit" "traperror" "trunc" "type" "typematch" "unames" "unbind" "`union`"             "userinfo"         "writeto" "`xor`" ))
;;    (8 .  ("`$`" "`*`" "`**`" "`+`"               "`<`" "`<=`" "`<>`" "`=`" "`>`" "`>=`"                        "`||`"              "`^`" "ASSERT" "Array" "ArrayOptions" "CopySign" "DEBUG" "Default0" "DefaultOverflow" "DefaultUnderflow" "ERROR" "EqualEntries" "EqualStructure" "FromInert" "Im" "MPFloat" "MorrBrilCull" "NextAfter"              "NumericClass" "NumericEvent" "NumericEventHandler" "NumericStatus"          "OrderedNE" "RETURN" "Re"                       "SFloatExponent" "SFloatMantissa" "Scale10" "Scale2" "SearchText" "TRACE" "ToInert" "Unordered"                "_jvm" "_maplet" "_treeMatch" "_unify" "_xml" "abs" "add" "addressof" "alias" "anames" "`and`" "andmap" "appendto" "array" "assemble" "assigned" "attributes" "bind"        "call_external" "callback" "cat" "coeff" "coeffs" "conjugate" "convert" "crinterp" "debugopts" "define_external" "degree" "denom" "diff" "disassemble" "divide" "dlclose" "`done`" "entries" "eval" "evalb" "evalf" "`evalf/hypergeom/kernel`" "evalgf1" "evalhf" "evaln" "expand" "exports" "factorial" "frem" "frontend" "gc" "genpoly"                    "goto" "has" "hastype" "hfarray" "icontent" "`if`" "igcd" "ilog10" "ilog2" "'implies'" "indets" "indices" "inner" "intersect" "`int/series`" "iolib" "iquo" "irem"          "isqrt" "kernelopts" "`kernel/transpose`" "lcoeff" "ldegree" "length" "lexorder" "lhs" "lprint" "macro" "map" "map2" "max" "maxnorm" "member" "min" "`minus`"         "modp" "modp1" "modp2" "mods" "mul" "mvMultiply" "negate" "nops" "normal" "`not`" "numboccur" "numer" "op" "`or`" "order" "ormap"            "parse"             "pointto" "print"                    "`quit`" "readlib"              "remove" "rhs" "rtable" "rtableInfo"                                                       "rtable_indfns" "rtable_is_zero" "rtable_normalize_index" "rtable_num_dims" "rtable_num_elems" "rtable_options"                               "rtable_scanblock"               "rtable_sort_indices"                        "searchtext" "select" "selectremove" "seq" "series" "setattribute"                    "sign" "sort"          "ssystem" "`stop`" "streamcall" "subs" "`subset`" "subsop" "substring" "system" "table" "taylor" "tcoeff" "time" "timelimit" "traperror" "trunc" "type" "typematch" "unames" "unbind" "`union`"             "userinfo"         "writeto" "`xor`" ))
;;    (9 .  ("`$`" "`*`" "`**`" "`+`" "`..`"        "`<`" "`<=`" "`<>`" "`=`" "`>`" "`>=`"                        "`||`"              "`^`" "ASSERT" "Array" "ArrayOptions" "CopySign" "DEBUG" "Default0" "DefaultOverflow" "DefaultUnderflow" "ERROR" "EqualEntries" "EqualStructure" "FromInert" "Im" "MPFloat" "MorrBrilCull" "NextAfter" "Normalizer" "NumericClass" "NumericEvent" "NumericEventHandler" "NumericStatus"          "OrderedNE" "RETURN" "Re"                       "SFloatExponent" "SFloatMantissa" "Scale10" "Scale2" "SearchText" "TRACE" "ToInert" "Unordered" "UpdateSource" "_jvm" "_maplet" "_treeMatch" "_unify" "_xml" "abs" "add" "addressof" "alias" "anames" "`and`" "andmap" "appendto" "array" "assemble" "assigned" "attributes" "bind"        "call_external" "callback" "cat" "coeff" "coeffs" "conjugate" "convert" "crinterp" "debugopts" "define_external" "degree" "denom" "diff" "disassemble" "divide" "dlclose" "`done`" "entries" "eval" "evalb" "evalf" "`evalf/hypergeom/kernel`" "evalgf1" "evalhf" "evaln" "expand" "exports" "factorial" "frem" "frontend" "gc" "genpoly" "gmp_isprime"      "goto" "has" "hastype" "hfarray" "icontent" "`if`" "igcd" "ilog10" "ilog2" "'implies'" "indets" "indices" "inner" "intersect" "`int/series`" "iolib" "iquo" "irem" "is_gmp" "isqrt" "kernelopts" "`kernel/transpose`" "lcoeff" "ldegree" "length" "lexorder" "lhs" "lprint" "macro" "map" "map2" "max" "maxnorm" "member" "min" "`minus`" "`mod`" "modp" "modp1" "modp2" "mods" "mul" "mvMultiply" "negate" "nops" "normal" "`not`" "numboccur" "numer" "op" "`or`" "order" "ormap"            "parse" "piecewise" "pointto" "print"                    "`quit`" "readlib" "reduce_opr" "remove" "rhs" "rtable" "rtableInfo"                      "rtable_eval"                    "rtable_indfns" "rtable_is_zero" "rtable_normalize_index" "rtable_num_dims" "rtable_num_elems" "rtable_options"                               "rtable_scanblock"               "rtable_sort_indices" "rtable_zip"           "searchtext" "select" "selectremove" "seq" "series" "setattribute"                    "sign" "sort"          "ssystem" "`stop`" "streamcall" "subs" "`subset`" "subsop" "substring" "system" "table" "taylor" "tcoeff" "time" "timelimit" "traperror" "trunc" "type" "typematch" "unames" "unbind" "`union`"             "userinfo"         "writeto" "`xor`" ))
;;    (10 . ("`$`" "`*`" "`**`" "`+`" "`..`"        "`<`" "`<=`" "`<>`" "`=`" "`>`" "`>=`" "`?()`" "`?[]`" "`{}`" "`||`"       "`[]`" "`^`" "ASSERT" "Array" "ArrayOptions" "CopySign" "DEBUG" "Default0" "DefaultOverflow" "DefaultUnderflow" "ERROR" "EqualEntries" "EqualStructure" "FromInert" "Im" "MPFloat" "MorrBrilCull" "NextAfter" "Normalizer" "NumericClass" "NumericEvent" "NumericEventHandler" "NumericStatus"          "OrderedNE" "RETURN" "Re"          "SDMPolynom" "SFloatExponent" "SFloatMantissa" "Scale10" "Scale2" "SearchText" "TRACE" "ToInert" "Unordered" "UpdateSource" "_jvm" "_maplet" "_treeMatch" "_unify" "_xml" "abs" "add" "addressof" "alias" "anames" "`and`" "andmap" "appendto" "array" "assemble" "assigned" "attributes" "bind"        "call_external" "callback" "cat" "coeff" "coeffs" "conjugate" "convert" "crinterp" "debugopts" "define_external" "degree" "denom" "diff" "disassemble" "divide" "dlclose" "`done`" "entries" "eval" "evalb" "evalf" "`evalf/hypergeom/kernel`" "evalgf1" "evalhf" "evaln" "expand" "exports" "factorial" "frem" "frontend" "gc" "genpoly" "gmp_isprime"      "goto" "has" "hastype" "hfarray" "icontent" "`if`" "igcd" "ilog10" "ilog2" "'implies'" "indets" "indices" "inner" "intersect" "`int/series`" "iolib" "iquo" "irem" "is_gmp" "isqrt" "kernelopts" "`kernel/transpose`" "lcoeff" "ldegree" "length" "lexorder" "lhs" "lprint" "macro" "map" "map2" "max" "maxnorm" "member" "min" "`minus`" "`mod`" "modp" "modp1" "modp2" "mods" "mul" "mvMultiply" "negate" "nops" "normal" "`not`" "numboccur" "numer" "op" "`or`" "order" "ormap" "overload" "parse" "piecewise" "pointto" "print"                    "`quit`" "readlib" "reduce_opr" "remove" "rhs" "rtable" "rtableInfo" "rtable_convolution" "rtable_eval" "rtable_histogram" "rtable_indfns" "rtable_is_zero" "rtable_normalize_index" "rtable_num_dims" "rtable_num_elems" "rtable_options" "rtable_redim" "rtable_scale" "rtable_scanblock"               "rtable_sort_indices" "rtable_zip" "savelib" "searchtext" "select" "selectremove" "seq" "series" "setattribute"                    "sign" "sort"          "ssystem" "`stop`" "streamcall" "subs" "`subset`" "subsop" "substring" "system" "table" "taylor" "tcoeff" "time" "timelimit" "traperror" "trunc" "type" "typematch" "unames" "unbind" "`union`"             "userinfo"         "writeto" "`xor`" ))
;;    (11 . ("`$`" "`*`" "`**`" "`+`" "`..`"        "`<`" "`<=`" "`<>`" "`=`" "`>`" "`>=`" "`?()`" "`?[]`" "`{}`" "`||`"       "`[]`" "`^`" "ASSERT" "Array" "ArrayOptions" "CopySign" "DEBUG" "Default0" "DefaultOverflow" "DefaultUnderflow" "ERROR" "EqualEntries" "EqualStructure" "FromInert" "Im" "MPFloat" "MorrBrilCull" "NextAfter" "Normalizer" "NumericClass" "NumericEvent" "NumericEventHandler" "NumericStatus"          "OrderedNE" "RETURN" "Re"          "SDMPolynom" "SFloatExponent" "SFloatMantissa" "Scale10" "Scale2" "SearchText" "TRACE" "ToInert" "Unordered" "UpdateSource" "_jvm" "_maplet" "_treeMatch" "_unify" "_xml" "abs" "add" "addressof" "alias" "anames" "`and`" "andmap" "appendto" "array" "assemble" "assigned" "attributes" "bind"        "call_external" "callback" "cat" "coeff" "coeffs" "conjugate" "convert" "crinterp" "debugopts" "define_external" "degree" "denom" "diff" "disassemble" "divide" "dlclose" "`done`" "entries" "eval" "evalb" "evalf" "`evalf/hypergeom/kernel`" "evalgf1" "evalhf" "evaln" "expand" "exports" "factorial" "frem" "frontend" "gc" "genpoly" "gmp_isprime"      "goto" "has" "hastype" "hfarray" "icontent" "`if`" "igcd" "ilog10" "ilog2" "'implies'" "indets" "indices" "inner" "intersect" "`int/series`" "iolib" "iquo" "irem" "is_gmp" "isqrt" "kernelopts" "`kernel/transpose`" "lcoeff" "ldegree" "length" "lexorder" "lhs" "lprint" "macro" "map" "map2" "max" "maxnorm" "member" "min" "`minus`" "`mod`" "modp" "modp1" "modp2" "mods" "mul" "mvMultiply" "negate" "nops" "normal" "`not`" "numboccur" "numer" "op" "`or`" "order" "ormap" "overload" "parse" "piecewise" "pointto" "print"                    "`quit`" "readlib" "reduce_opr" "remove" "rhs" "rtable" "rtableInfo" "rtable_convolution" "rtable_eval" "rtable_histogram" "rtable_indfns" "rtable_is_zero" "rtable_normalize_index" "rtable_num_dims" "rtable_num_elems" "rtable_options" "rtable_redim" "rtable_scale" "rtable_scanblock"               "rtable_sort_indices" "rtable_zip" "savelib" "searchtext" "select" "selectremove" "seq" "series" "setattribute"                    "sign" "sort"          "ssystem" "`stop`" "streamcall" "subs" "`subset`" "subsop" "substring" "system" "table" "taylor" "tcoeff" "time" "timelimit" "traperror" "trunc" "type" "typematch" "unames" "unbind" "`union`"             "userinfo"         "writeto" "`xor`" ))
;;    (12 . ("`$`" "`*`" "`**`" "`+`" "`..`"        "`<`" "`<=`" "`<>`" "`=`" "`>`" "`>=`" "`?()`" "`?[]`" "`{}`" "`||`"       "`[]`" "`^`" "ASSERT" "Array" "ArrayOptions" "CopySign" "DEBUG" "Default0" "DefaultOverflow" "DefaultUnderflow" "ERROR" "EqualEntries" "EqualStructure" "FromInert" "Im" "MPFloat" "MorrBrilCull" "NextAfter" "Normalizer" "NumericClass" "NumericEvent" "NumericEventHandler" "NumericStatus"          "OrderedNE" "RETURN" "Re" "Record" "SDMPolynom" "SFloatExponent" "SFloatMantissa" "Scale10" "Scale2" "SearchText" "TRACE" "ToInert" "Unordered" "UpdateSource" "_jvm" "_maplet" "_treeMatch" "_unify" "_xml" "abs" "add" "addressof" "alias" "anames" "`and`" "andmap" "appendto" "array" "assemble" "assigned" "attributes" "bind"        "call_external" "callback" "cat" "coeff" "coeffs" "conjugate" "convert" "crinterp" "debugopts" "define_external" "degree" "denom" "diff" "disassemble" "divide" "dlclose" "`done`" "entries" "eval" "evalb" "evalf" "`evalf/hypergeom/kernel`" "evalgf1" "evalhf" "evaln" "expand" "exports" "factorial" "frem" "frontend" "gc" "genpoly" "gmp_isprime"      "goto" "has" "hastype" "hfarray" "icontent" "`if`" "igcd" "ilog10" "ilog2" "'implies'" "indets" "indices" "inner" "intersect" "`int/series`" "iolib" "iquo" "irem" "is_gmp" "isqrt" "kernelopts" "`kernel/transpose`" "lcoeff" "ldegree" "length" "lexorder" "lhs" "lprint" "macro" "map" "map2" "max" "maxnorm" "member" "min" "`minus`" "`mod`" "modp" "modp1" "modp2" "mods" "mul" "mvMultiply" "negate" "nops" "normal" "`not`" "numboccur" "numer" "op" "`or`" "order" "ormap" "overload" "parse" "piecewise" "pointto" "print"                    "`quit`" "readlib" "reduce_opr" "remove" "rhs" "rtable" "rtableInfo" "rtable_convolution" "rtable_eval" "rtable_histogram" "rtable_indfns" "rtable_is_zero" "rtable_normalize_index" "rtable_num_dims" "rtable_num_elems" "rtable_options" "rtable_redim" "rtable_scale" "rtable_scanblock" "rtable_size" "rtable_sort_indices" "rtable_zip" "savelib" "searchtext" "select" "selectremove" "seq" "series" "setattribute"                    "sign" "sort"          "ssystem" "`stop`" "streamcall" "subs" "`subset`" "subsop" "substring" "system" "table" "taylor" "tcoeff" "time" "timelimit" "traperror" "trunc" "type" "typematch" "unames" "unbind" "`union`"             "userinfo"         "writeto" "`xor`" ))
;;    (13 . ("`$`" "`*`" "`**`" "`+`" "`..`" "`::`" "`<`" "`<=`" "`<>`" "`=`" "`>`" "`>=`" "`?()`" "`?[]`" "`{}`" "`||`" "`~`" "`[]`" "`^`" "ASSERT" "Array" "ArrayOptions" "CopySign" "DEBUG" "Default0" "DefaultOverflow" "DefaultUnderflow" "ERROR" "EqualEntries" "EqualStructure" "FromInert" "Im" "MPFloat" "MorrBrilCull" "NextAfter" "Normalizer" "NumericClass" "NumericEvent" "NumericEventHandler" "NumericStatus"          "OrderedNE" "RETURN" "Re" "Record" "SDMPolynom" "SFloatExponent" "SFloatMantissa" "Scale10" "Scale2" "SearchText" "TRACE" "ToInert" "Unordered" "UpdateSource" "_jvm" "_maplet" "_treeMatch" "_unify" "_xml" "abs" "add" "addressof" "alias" "anames" "`and`" "andmap" "appendto" "array" "assemble" "assigned" "attributes" "bind"        "call_external" "callback" "cat" "coeff" "coeffs" "conjugate" "convert" "crinterp" "debugopts" "define_external" "degree" "denom" "diff" "disassemble" "divide" "dlclose" "`done`" "entries" "eval" "evalb" "evalf" "`evalf/hypergeom/kernel`" "evalgf1" "evalhf" "evaln" "expand" "exports" "factorial" "frem" "frontend" "gc" "genpoly" "gmp_isprime"      "goto" "has" "hastype" "hfarray" "icontent" "`if`" "igcd" "ilog10" "ilog2" "`implies`" "indets" "indices" "inner" "intersect" "`int/series`" "iolib" "iquo" "irem" "is_gmp" "isqrt" "kernelopts" "`kernel/transpose`" "lcoeff" "ldegree" "length" "lexorder" "lhs" "lprint" "macro" "map" "map2" "max" "maxnorm" "member" "min" "`minus`" "`mod`" "modp" "modp1" "modp2" "mods" "mul" "mvMultiply" "negate" "nops" "normal" "`not`" "numboccur" "numer" "op" "`or`" "order" "ormap" "overload" "parse" "piecewise" "pointto" "print"                    "`quit`" "readlib" "reduce_opr" "remove" "rhs" "rtable" "rtableInfo" "rtable_convolution" "rtable_eval" "rtable_histogram" "rtable_indfns" "rtable_is_zero" "rtable_normalize_index" "rtable_num_dims" "rtable_num_elems" "rtable_options" "rtable_redim" "rtable_scale" "rtable_scanblock" "rtable_size" "rtable_sort_indices" "rtable_zip" "savelib" "searchtext" "select" "selectremove" "seq" "series" "setattribute"                    "sign" "sort"          "ssystem" "`stop`" "streamcall" "subs" "`subset`" "subsop" "substring" "system" "table" "taylor" "tcoeff" "time" "timelimit" "traperror" "trunc" "type" "typematch" "unames" "unbind" "`union`"             "userinfo"         "writeto" "`xor`" ))
;;    (14 . ("`$`" "`*`" "`**`" "`+`" "`..`" "`::`" "`<`" "`<=`" "`<>`" "`=`" "`>`" "`>=`" "`?()`" "`?[]`" "`{}`" "`||`" "`~`" "`[]`" "`^`" "ASSERT" "Array" "ArrayOptions" "CopySign" "DEBUG" "Default0" "DefaultOverflow" "DefaultUnderflow" "ERROR" "EqualEntries" "EqualStructure" "FromInert" "Im" "MPFloat" "MorrBrilCull" "NextAfter" "Normalizer" "NumericClass" "NumericEvent" "NumericEventHandler" "NumericStatus" "Object" "OrderedNE" "RETURN" "Re" "Record" "SDMPolynom" "SFloatExponent" "SFloatMantissa" "Scale10" "Scale2" "SearchText" "TRACE" "ToInert" "Unordered" "UpdateSource" "_jvm" "_maplet" "_treeMatch" "_unify" "_xml" "abs" "add" "addressof" "alias" "anames" "`and`" "andmap" "appendto" "array" "assemble" "assigned" "attributes" "bind"        "call_external" "callback" "cat" "coeff" "coeffs" "conjugate" "convert" "crinterp" "debugopts" "define_external" "degree" "denom" "diff" "disassemble" "divide" "dlclose" "`done`" "entries" "eval" "evalb" "evalf" "`evalf/hypergeom/kernel`" "evalgf1" "evalhf" "evaln" "expand" "exports" "factorial" "frem" "frontend" "gc" "genpoly" "gmp_isprime"      "goto" "has" "hastype" "hfarray" "icontent" "`if`" "igcd" "ilog10" "ilog2" "`implies`" "indets" "indices" "inner" "intersect" "`int/series`" "iolib" "iquo" "irem" "is_gmp" "isqrt" "kernelopts" "`kernel/transpose`" "lcoeff" "ldegree" "length" "lexorder" "lhs" "lprint" "macro" "map" "map2" "max" "maxnorm" "member" "min" "`minus`" "`mod`" "modp" "modp1" "modp2" "mods" "mul" "mvMultiply" "negate" "nops" "normal" "`not`" "numboccur" "numer" "op" "`or`" "order" "ormap" "overload" "parse" "piecewise" "pointto" "print"                    "`quit`" "readlib" "reduce_opr" "remove" "rhs" "rtable" "rtableInfo" "rtable_convolution" "rtable_eval" "rtable_histogram" "rtable_indfns" "rtable_is_zero" "rtable_normalize_index" "rtable_num_dims" "rtable_num_elems" "rtable_options" "rtable_redim" "rtable_scale" "rtable_scanblock" "rtable_size" "rtable_sort_indices" "rtable_zip" "savelib" "searchtext" "select" "selectremove" "seq" "series" "setattribute"                    "sign" "sort"          "ssystem" "`stop`" "streamcall" "subs" "`subset`" "subsop" "substring" "system" "table" "taylor" "tcoeff" "time" "timelimit" "traperror" "trunc" "type" "typematch" "unames" "unbind" "`union`"             "userinfo"         "writeto" "`xor`" ))
;;  "Alist of Maple builtin funtions. The key is the major release."))

;;}}}

(defconst maplev--ditto-operators-re
  (eval-when-compile (regexp-opt '("%" "%%" "%%%")))
  "Return a regexp that matches the ditto operators.")

(defvar maplev-protected-face   'maplev-protected-face
  "*Face name for Maple protected names.")

(defface maplev-protected-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))  (:foreground "DimGray"   :bold t))
    (((class color)     (background light)) (:foreground "LimeGreen"))
    (((class color)     (background dark))  (:foreground "LimeGreen"))
    (t (:bold t)))
  "Font lock mode face used for Maple protected names."
  :group 'maplev-faces)

(eval-and-compile
  (defconst maplev--protected-names
    (list
     ;; constants
     "Catalan" "FAIL" "Pi" "false" "gamma" "infinity" "true"

     ;; interface options
     "ansi" "echo" "errorbreak" "errorcursor" "indentamount" "labeling"
     "labelwidth" "patchlevel" "plotdevice" "plotoptions" "plotoutput"
     "postplot" "preplot" "prettyprint" "printbytes" "prompt" "quiet"
     "screenheight" "screenwidth" "showassumed" "verboseproc" "version"
     "warnlevel"
     
     ;; kernelopts options
     "ASSERT" "bytesalloc" "bytesused" "cputime" "dagtag" "gcbytesavail"
     "gcbytesreturned" "gctimes" "maxdigits" "maximmediate" "memusage"
     "printbytes" "profile" "system" "version" "wordsize"
     
     ;; types
     "_Inert" "And" "Non" "Not" "Or" "SERIES" "SymbolicInfinity" "TEXT"
     "algebraic" "algext" "algfun" "algnum" "algnumext"
     "anyfunc" "anything" "arctrig" "atomic"
     "boolean" "complex" "complexcons" "constant" "cubic"
     "cx_infinity" "cx_zero" "embedded_axis" "embedded_imaginary" "embedded_real"
     "equation" "even" "evenfunc" "expanded" "extended_numeric" "extended_rational"
     "facint" "finite" "float" "fraction" "function" "hfloat"
     "identical" "imaginary" "indexable" "indexed" "infinity" "integer"
     "laurent" "linear" "list" "listlist" "literal" "logical" "mathfunc" "matrix"
     "moduledefinition" "monomial" "name" "neg_infinity"
     "negative" "negint" "negzero" "nonnegative" "nonnegint" "nonposint"
     "nonpositive" "nonreal" "nothing" "numeric" "odd" "oddfunc" "package"
     "point" "polynom" "pos_infinity" "posint" "positive" "poszero" "prime"
     "protected" "quadratic" "quartic" "radext" "radfun" "radfunext"
     "radical" "radnum" "radnumext" "range" "rational" "ratpoly" "real_infinity"
     "realcons" "relation" "scalar" "sequential" "set" "sfloat" "specfunc" "specindex" "sqrt"
     "stack" "string" "symbol" "symmfunc" "tabular" "trig" "truefalse" "truefalseFAIL"
     "undefined" "uneval" "vector" "zppoly"
     
     ;; math procedures
     ;; Some of these were obtained with
     ;; ListTools:-MakeUnique(sort(map(op@FunctionAdvisor, FunctionAdvisor(function_classes)))); 
     "about" "abs" "addcoords" "additionally" "addproperty" "AFactor" "AFactors" "AiryAi"
     "AiryAiZeros" "AiryBi" "AiryBiZeros" "algsubs" "alias" "allvalues" "andseq" "AngerJ"
     "AppellF1" "AppellF2" "AppellF3" "AppellF4" "apply" "applyop" "applyrule" "arccos"
     "arccosh" "arccot" "arccoth" "arccsc" "arccsch" "arcsec" "arcsech" "arcsin" "arcsinh"
     "arctan" "arctanh" "argument" "ArrayDims" "ArrayElems" "ArrayIndFns" "ArrayNumDims"
     "assume" "asympt" "BellB" "Berlekamp" "bernoulli" "bernstein" "BesselI" "BesselJ"
     "BesselJZeros" "BesselK" "BesselY" "BesselYZeros" "Beta" "binomial" "branches"
     "Cache" "ceil" "charfcn" "ChebyshevT" "ChebyshevU" "CheckArgs" "Chi" "chrem" "Ci"
     "coeftayl" "collect" "combine" "comparray" "compiletable" "CompleteBellB" "Complex"
     "ComplexRange" "compoly" "conjugate" "Content" "content" "convergs" "copy" "cos"
     "cosh" "cot" "coth" "coulditbe" "CoulombF" "csc" "csch" "CylinderD" "CylinderU"
     "CylinderV" "D" "dataplot" "dawson" "define" "definemore" "depends" "Describe"
     "DESol" "Det" "Diff" "dilog" "dims" "dinterp" "Dirac" "discont" "discrim" "dismantle"
     "DistDeg" "Divide" "doublefactorial" "dsolve" "Ei" "elems" "eliminate" "ellipsoid"
     "EllipticCE" "EllipticCK" "EllipticCPi" "EllipticE" "EllipticF" "EllipticK"
     "EllipticModulus" "EllipticNome" "EllipticPi" "erf" "erfc" "erfi" "euler"
     "eulermac" "Eval" "evala" "evalapply" "evalc" "evalr" "evalrC" "example"
     "exists" "exp" "Expand" "Explore" "ExportVector" "extrema" "Factor"
     "factor" "factorial" "Factors" "factors" "fdiscont" "fixdiv" "floor"
     "fnormal" "forall" "forget" "fourier" "fouriercos" "fouriersin" "frac"
     "Fraction" "freeze" "FresnelC" "Fresnelf" "Fresnelg" "FresnelS" "fsolve"
     "galois" "GAMMA" "GaussAGM" "Gausselim" "Gaussjord" "Gcd" "gcd" "Gcdex" "gcdex"
     "GegenbauerC" "GeneralizedPolylog" "getassumptions" "GF" "hankel" "HankelH1"
     "HankelH2" "harmonic" "hasassumptions" "hasfun" "hasoption" "Heaviside" "help"
     "Hermite" "HermiteH" "HeunB" "HeunBPrime" "HeunC" "HeunCPrime" "HeunD" "HeunDPrime"
     "HeunG" "HeunGPrime" "HeunT" "HeunTPrime" "HFloat" "hilbert" "history" "hypergeom"
     "identify" "ifactor" "ifactors" "igcdex" "ilcm" "ilog" "Im" "implicitdiff"
     "ImportVector" "IncompleteBellB" "Indep" "index" "info" "initialcondition"
     "insertpattern" "Int" "int" "Intat" "intat" "Interp" "interp" "intsolve"
     "InverseJacobiAM" "InverseJacobiCD" "InverseJacobiCN" "InverseJacobiCS"
     "InverseJacobiDC" "InverseJacobiDN" "InverseJacobiDS" "InverseJacobiNC"
     "InverseJacobiND" "InverseJacobiNS" "InverseJacobiSC" "InverseJacobiSD"
     "InverseJacobiSN" "invfourier" "invfunc" "invhilbert" "invlaplace" "invmellin"
     "invztrans" "iperfpow" "iratrecon" "iroot" "Irreduc" "irreduc" "is" "iscont"
     "IsMatrixShape" "isolate" "isolve" "ispoly" "isprime" "isqrfree" "issqr"
     "IsVectorShape" "IsWorksheetInterface" "ithprime" "JacobiAM" "JacobiCD"
     "JacobiCN" "JacobiCS" "JacobiDC" "JacobiDN" "JacobiDS" "JacobiNC" "JacobiND"
     "JacobiNS" "JacobiP" "JacobiSC" "JacobiSD" "JacobiSN" "JacobiTheta1" "JacobiTheta2"
     "JacobiTheta3" "JacobiTheta4" "JacobiZeta" "KelvinBei" "KelvinBer" "KelvinHei"
     "KelvinHer" "KelvinKei" "KelvinKer" "KummerM" "KummerU" "LaguerreL" "LambertW"
     "laplace" "latex" "Lcm" "lcm" "leadterm" "LegendreP" "LegendreQ" "LerchPhi" "Li"
     "Limit" "limit" "ln" "lnGAMMA" "log" "log10" "log2" "LommelS1" "LommelS2"
     "maptype" "match" "MathieuA" "MathieuB" "MathieuC" "MathieuCE" "MathieuCEPrime"
     "MathieuCPrime" "MathieuExponent" "MathieuFloquet" "MathieuFloquetPrime" "MathieuS"
     "MathieuSE" "MathieuSEPrime" "MathieuSPrime" "Matrix" "MatrixOptions" "max" "maximize"
     "MeijerG" "mellin" "min" "minimize" "modpol" "MOLS" "msolve" "mtaylor" "multinomial"
     "MultiPolylog" "MultiZeta" "nextprime" "NielsenPolylog" "norm" "Normal" "nprintf"
     "Nullspace" "odetest" "orseq" "packages" "patmatch" "piecewise" "plot" "plot3d"
     "plotsetup" "pochhammer" "poisson" "polylog" "Power" "Powmod" "powmod" "Prem"
     "prem" "prevprime" "Primfield" "Primitive" "primpart" "printf" "Product" "product"
     "proot" "protect" "Psi" "psqrt" "Quo" "quo" "radfield" "radnormal" "rand" "randomize"
     "Randpoly" "randpoly" "rationalize" "Ratrecon" "ratrecon" "Re" "readdata" "readstat"
     "RealRange" "realroot" "redefine" "reduce" "related" "Rem" "rem" "residue" "RESol"
     "Resultant" "resultant" "root" "rootbound" "Roots" "roots" "round" "rsolve" "rtable"
     "scanf" "sec" "sech" "selectfun" "shake" "Shi" "showtime" "Si" "signum" "simplify"
     "sin" "singular" "sinh" "sinterp" "smartplot" "smartplot3d" "Smith" "solve"
     "SphericalY" "sprem" "sprintf" "Sqrfree" "sqrfree" "sscanf" "Ssi" "Stirling1"
     "Stirling2" "String" "StruveH" "StruveL" "sturm" "sturmseq" "subtype" "Sum" "sum"
     "surd" "symmdiff" "tablelook" "tan" "tanh" "testeq" "thaw" "TopologicalSort" "Trace"
     "trigsubs" "unapply" "unassign" "undefine" "unprotect" "unwindK" "usage" "value" "Vector"
     "verify" "version" "WARNING" "WeberE" "WeierstrassP" "WeierstrassPPrime"
     "WeierstrassSigma" "WeierstrassZeta" "whattype" "WhittakerM" "WhittakerW"
     "Wrightomega" "xormap" "xorseq" "Zeta" "ztrans"
     
     ;; miscellaneous procedures
     "interface" "readline" "with" "unwith"
     )
    "List of some of the protected names in Maple.
This is supposed to exclude the builtins and reserved words."))

(defconst maplev--protected-names-re
  (eval-when-compile
    (concat "\\<" (regexp-opt maplev--protected-names) "\\>"))
  "Regular expression matching Maple protected names.")


(defvar maplev-undocumented-face   'maplev-undocumented-face
  "*Face name for Maple undocumented names.")

(defface maplev-undocumented-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))  (:foreground "DimGray"   :bold t))
    (((class color)     (background light)) (:foreground "VioletRed1"))
    (((class color)     (background dark))  (:foreground "VioletRed1"))
    (t (:bold t)))
  "Font lock mode face used for Maple undocumented names."
  :group 'maplev-faces)

(defconst maplev--undocumented-names-re
  (eval-when-compile
    (concat "\\<\\(?:"
	    (regexp-opt
	     (list
	      "_a" "_b" "_c" "_X" "_x" "_y" "_Z" "_z"
	      "abnd" "ARRAY" "assignfcn" "bind" "cmagdiff" "crinterp"
	      "DAESimplify" "Discrim" "dlclose" "EqualStructure" "evalgf1"
	      "fgbrs" "FRAMESCALING" "GetAlgExt" "gmp_isprime" "InertForms"
	      "InertNames" "inner" "is_gmp" "libmgb" "MorrBrilCull" "MPFloat"
	      "mvMultiply" "negate" "NONUNIFORM" "NumericTools" "PackageManagement"
	      "PatternMatching" "PIECEWISE" "PiecewiseTools" "PseudoStack" "PuiseuxSeries"
	      "reduce_opr" "RestoreSession" "rtable_convolution" "rtable_histogram"
	      "rtable_is_zero" "rtable_normalize_index" "rtable_scale" "rtable_sort_indices"
	      "rtable_zip" "rtableInfo" "SaveSession" "sdmp" "Subres" "TABLE" "TestTools"
	      "TRACE" "unbind" "UNIFORM" "UpdateSource" "VerifyTools"
	      ))
	    "\\)\\>"))
  "List of undocumented names reserved for internal use.")


(defun maplev-font-lock-keywords-1 ()
  "Compute the minimum decoration `font-lock-keywords' for MapleV mode.
Top level procedures, Maple reserved words, and preprocessor directives
are font locked."
  (list
   (list maplev--top-defun-begin-re '(1 font-lock-function-name-face t))
   (list maplev--preprocessor-directives-re '(0 maplev-preprocessor-face))
   (list (maplev--list-to-word-re maplev-reserved-words)
         '(0 font-lock-keyword-face))))


(defun maplev-font-lock-keywords-2 ()
  "Compute the medium decoration `font-lock-keywords' for MapleV mode.
Add special words, initial variables, and the ditto operators to the
minimum decoration keywords."
  (append
   (maplev-font-lock-keywords-1)
   (list
    (list maplev--special-words-re     '(0 font-lock-variable-name-face))
    (list maplev--initial-variables-re '(0 font-lock-reference-face))
    (list maplev--ditto-operators-re   '(0 font-lock-variable-name-face)))))

(defun maplev-font-lock-keywords-3 ()
  "Compute the maximum decoration `font-lock-keywords' for MapleV mode.
Add builtin functions to the medium decoration keywords."
  (let ((max-specpdl-size 10000))       ; default 600 is too small
    (append (maplev-font-lock-keywords-2)
            (list (list (maplev--list-to-word-re (append maplev-builtin-functions
							 maplev-builtin-types
							 maplev-constructors))
                        '(0 font-lock-builtin-face))
                  (list maplev--deprecated-re '(0 font-lock-warning-face))
                  (list maplev--protected-names-re '(0 maplev-protected-face))
                  (list maplev--undocumented-names-re '(0 maplev-undocumented-face))))))

(defun maplev--font-lock-keywords ()
  "Return a list of symbols for font locking MapleV mode buffers."
  '(maplev-font-lock-keywords-3        ; default is maximum decoration
    maplev-font-lock-keywords-1
    maplev-font-lock-keywords-2
    maplev-font-lock-keywords-3))

(defun maplev--syntax-begin ()
  "Move backwards to start of a Maple procedure.
This is passed to `font-lock-defaults' as the SYNTAX-BEGIN argument."
  (re-search-backward maplev--top-defun-begin-re nil 'move))

(defun maplev-reset-font-lock (&optional decoration)
  "Reset the font lock patterns for MapleV mode.  Fontify the buffer.
The optional argument DECORATION selects the level of font lock.
If nil then `font-lock-maximum-decoration' selects the level."
  (interactive (list (completing-read "Decoration (1-3): "
                                      '(("1") ("2") ("3"))
                                      nil t)))
  (if decoration
      (setq font-lock-maximum-decoration decoration))
  (setq font-lock-defaults `(,(maplev--font-lock-keywords)
                             nil nil
                             ((?_ . "w")) ; make underscore a word constituent
                             maplev--syntax-begin))
  (font-lock-set-defaults)
  (font-lock-ensure))

;;}}}
;;{{{ Tags

;; I'm not sure about how tags should work.  Should it run on all
;; Maple files in the directory?  Running it on just one file makes
;; little sense.  The tags could be appended, but then the TAGS file
;; will have lots of redunancy following multiple executions.

;; (defcustom maplev-etags "etags"
;;   "Etag program."
;;   :type 'string
;;   :group 'maplev)

;; (defcustom maplev-tag-regexp
;;   (concat "'/\\([^# \t]+\\)[ \t]*:=[ \t]*proc(/\\1/'")
;;   "Regular expression used by etag."
;;   :type 'string
;;   :group 'maplev)

;; ;; where does the following store the tag table?
;; ;; Always in the same directory as the

;; (defun maplev-tag-file ()
;;   "Create a tags table for the existing buffer/file."
;;   (interactive)
;;   (shell-command
;;    (concat maplev-etags
;;         " --language=none --regex="
;;         maplev-tag-regexp
;;         " "
;;         (buffer-file-name))))

;;}}}

;;{{{ Includes

(defface maplev-find-include-file
  '((((class grayscale) (background light)) (:foreground "LightGray" :underline t))
    (((class grayscale) (background dark))  (:foreground "DarkGray" :underline t))
    (((class color)     (background light)) (:foreground "DarkBlue" :underline t))
    (((class color)     (background dark))  (:foreground "LightBlue" :underline t))
    (t (:underline t)))
  "Font lock face used for include filenames, indicates hyperlink."
  :group 'maplev-faces)

(defun maplev-buttonize-includes ()
  "Buttonize the include statements."
  (button-lock-mode t)
  (button-lock-set-button maplev--include-directive-re
			  'maplev-find-include-file-at-point
			  :face 'link
			  :face-policy 'prepend
			  :grouping 2
			  :keyboard-binding "C-c C-o"
			  :help-text "open file ([C-u] C-c C-o)"))

(defun maplev-find-include-file-at-point (toggle)
  "Open the include file at point.
If found, the file is opened either in this window or the other
window, depending on the exclusive-or of TOGGLE with
`maplev-include-file-other-window-flag'.  The :include-path slot
of variable `maplev-config' specifies the search paths.  If the file
cannot be found, but the proper directory exists, query user to
create the file."
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (unless (looking-at maplev--include-directive-re)
      (error "Not at an include statement"))
    (let* ((inc-file (match-string-no-properties 2))
	   (path (slot-value maplev-config 'include-path))
	   (inc-first (string= "<" (match-string-no-properties 1)))
	   (other-window-flag (if maplev-include-file-other-window-flag
				  (not toggle)
				toggle))
	   file)
      (setq file (maplev-find-include-file inc-file path inc-first))
      (if file
	  (if other-window-flag
	      (find-file-other-window file )
	    (find-file file))
	;; file does not exist.  If suitable location can be found from include path,
	;; query to create
	(let ((base (file-name-nondirectory inc-file))
	      (inc-dir inc-file))
	  (while (and (setq inc-dir (file-name-directory (directory-file-name inc-dir)))
		      (not (setq file (maplev-find-include-file inc-dir path inc-first)))))
	  (if (not file)
	      (error "Include file %s does not exist " inc-file)
	    (when (yes-or-no-p (format "Create include file %s "
				       (setq file (concat file base))))
	      (if other-window-flag
		  (find-file-other-window file)
		(find-file file)))))))))

(defun maplev-find-include-file (inc-file &optional inc-path inc-first)
  "Find the Maple include file INC-FILE and return as an absolute path.
INC-PATH is an optional list of rooted directories.  Use each
directory, in order, as parent of INC-FILE.  If INC-FIRST is
non-nil, search the INC-PATH directories before using the
`default-directory'.  If those searches fail, search each parent
of `default-directory'.  Return nil if the file is not found."
  (if (file-name-absolute-p inc-file)
      (and (file-exists-p inc-file) inc-file)
    (if (and inc-path inc-first)
	(or
	 (maplev-include--find-file-in-path inc-file inc-path)
	 (maplev-include--find-file-up-path inc-file))
      (or (maplev-include--find-file-in-path inc-file (list default-directory))
	  (and inc-path (maplev-include--find-file-in-path inc-file inc-path))
	  (maplev-include--find-file-up-path inc-file)))))

(defun maplev-include--find-file-in-path (file paths)
  "Search for FILE in a list of rooted PATHS, which include trailing slash.
If found, return the absolute path to FILE, otherwise return nil."
  (let (dir abs-file)
    (while (not (progn
		  (setq dir (file-name-as-directory (car paths))
			paths (cdr paths)
			abs-file (concat dir file))
		  (or (file-exists-p abs-file)
		      (setq abs-file nil)
		      (null paths)))))
    (and abs-file
	 (expand-file-name abs-file))))

(defun maplev-include--find-file-up-path (file &optional dir)
  "Find FILE, optionally searching in directory DIR.
Look in each ancestor in DIR.  If DIR is nil, use `default-directory'.
Return the absolute path to the file, if found, otherwise return
nil."
  (setq dir (file-name-as-directory (or dir default-directory)))
  (let (parent abs-file)
    (while
	(if (file-exists-p (setq abs-file (concat dir file)))
	    nil ; success; exit loop
	  (if (or (null (setq parent (file-name-directory (directory-file-name dir))))
		  (string= dir parent))
	      (setq abs-file nil) ; at root, exit loop with empty file
	    (setq dir parent)))) ; check parent
    abs-file))

(define-button-type 'maplev-find-include-file
  'help-echo "Find include file"
  'action 'maplev-find-include-file-at-point
  'follow-link t
  'face 'maplev-include-file)

;;}}}
;;{{{ Links

(defun maplev-buttonize-links ()
  "Buttonize the link statements."
  (button-lock-mode t)
  (button-lock-set-button maplev--link-re
			  'maplev-find-link-file-at-point
			  :face 'link
			  :face-policy 'prepend
			  :grouping 1
			  :keyboard-binding "C-c C-o"
			  :help-text "open file"))


(defun maplev-find-link-file-at-point (toggle)
  "Open the maplev link file at point, expanding any environment variables.
If found, the file is opened in the current window, or the other
window, depending on the exclusive-or of
`maplev-include-file-other-window-flag' and TOGGLE."
  (interactive "P")
  (unless (eolp)
    (save-excursion
      (beginning-of-line)
      (unless (looking-at maplev--link-re)
	(error "Not at a link statement"))
      (let* ((link-file (match-string-no-properties 1))
	     (file (expand-file-name (substitute-in-file-name link-file))))
	(unless (file-exists-p file)
	  (error "Cannot find link file %s" link-file))
	(if (if maplev-include-file-other-window-flag
		(not toggle)
	      toggle)
	    (find-file-other-window file)
	  (find-file file))))))


;;}}}

;;{{{ Config file (.maplev)

(defun maplev-find-config-file ()
  "Find and open the maple configuration file.
The file is named .maplev and is searched for in the current
directory and its ancestors."
  (interactive)
  (let ((config (maplev-include--find-file-up-path ".maplev")))
    (if config
	(find-file config)
      (message "Could not find .maplev file"))))

(defun maplev-load-config-file ()
  "Find and load the maplev configuration file.
The file is named .maplev and is searched for in the current
directory and its ancestors.  Return the path to the configuration
file if one was found, nil otherwise."
  (interactive)
  (let ((maplev-config-file (maplev-include--find-file-up-path ".maplev")))
    (when maplev-config-file
      (condition-case err
	  (progn
	    (load maplev-config-file)
	    maplev-config-file)
	(error
	 (error "Problem loading config file %s: %s" maplev-config-file err))))))

;;}}}

;;{{{ leading-comma stuff

(defadvice fixup-whitespace (after maplev-fixup-whitespace)
  "Catenate adjacent Maple strings (separated by one space) or,
if `maplev-leading-comma-flag' is non-nil, remove space before a comma."
  (if (and maplev-leading-comma-flag
	   (looking-at " ,"))
      (delete-char 1)
    (when (and (looking-at " \"")
	       (looking-back "\"" nil))
      (delete-char -1)
      (delete-char 2))))

;;}}}

;;{{{ Frames

;; The following is a slightly modified version of
;; `mouse-tear-off-window' from mouse.el.

(defun maplev-tear-off-window ()
  "Delete the current window and create a new frame displaying its buffer."
  (interactive)
  (if (one-window-p t 'here)
      (message "Only one window in frame.")
    (let* ((window (selected-window))
           (buf (window-buffer window))
           (frame (make-frame)))
      (select-frame frame)
      (switch-to-buffer buf)
      (delete-window window))))

;;}}}


(provide 'maplev)
(provide 'maplev-mode)

;;; maplev.el ends here
