;;; maplev-mint.el --- Syntax checking for Maple

;;; Commentary:
;; 

;;; Code:
;;

(require 'eieio)
(require 'maplev-config)
(require 'maplev-custom)
(require 'maplev-re)

(eval-when-compile
  (defvar maplev-add-declaration-function)
  (defvar maplev-alphabetize-declarations-p)
  (defvar maplev-var-declaration-symbol)
  (defvar maplev-variable-spacing)
  (defvar maplev-mode-syntax-table)
  (defvar maplev-quote-not-string-syntax-table)
  (defvar maplev-symbol-syntax-table))

(declare-function event-point "maplev-common")
(declare-function event-window "maplev-common")
(declare-function maplev--end-of-defun-pos "maplev-common")
(declare-function maplev--ident-around-point "maplev-common")
(declare-function maplev-beginning-of-defun "maplev-common")
(declare-function maplev-current-defun "maplev-common")
(declare-function maplev-find-include-file "maplev")
(declare-function maplev-forward-expr "maplev")
(declare-function maplev-ident-around-point-interactive "maplev-common")
(declare-function maplev-indent-line "maplev-indent")
(declare-function maplev-indent-newline "maplev")
(declare-function maplev-expand->file-name "maplev")

;;{{{ customizable variables

(defcustom maplev-mint-query-flag t
  "Non-nil means query before correcting."
  :type 'boolean
  :group 'maplev-mint)

(defcustom maplev-mint-rerun-flag nil
  "Non-nil means rerun mint after each operation."
  :type 'boolean
  :group 'maplev-mint)

(defcustom maplev-mint-save-rerun-flag nil
  "Non-nil means save a source buffer when using mint to modify it.
This is only used if `maplev-mint-rerun-flag' is non-nil."
  :type 'boolean
  :group 'maplev-mint)

;;}}}
;;{{{ variables

(defvar maplev-mint--code-buffer nil
  "Buffer containing source code that was passed to Mint.")

(defvar maplev-mint--code-beginning nil
  "Marker at beginning of region in `maplev-mint--code-buffer' that was passed to Mint.")

(defvar maplev-mint--code-end nil
  "Marker at end of region in `maplev-mint--code-buffer' that was passed to Mint.")

(defvar maplev--declaration-history nil
  "History list used for type declarations.")

;;}}}
;;{{{ syntax table

(defvar maplev-mint-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\[ "w"  table)
    (modify-syntax-entry ?\] "w"  table)
    (modify-syntax-entry ?_  "w"  table)
    (modify-syntax-entry ?/  "w"  table)
    (modify-syntax-entry ?\` "\"" table) ; string quotes
    table)
  "Syntax table used in Maple mint buffer.")

;;}}}
;;{{{ mode map

(defvar maplev-mint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(space)]                     'scroll-up)
    (define-key map [(backspace)]                 'scroll-down)
    (define-key map [(return)]                    'maplev-mint-rerun)
    (define-key map [(control c) (return) return] 'maplev-mint-rerun)
    (define-key map [?q]                          'quit-window)
    (define-key map [?s]                          'isearch-forward)
    (define-key map [?r]                          'isearch-backward)
    (define-key map [(mouse-1)]                   'maplev-mint-click)
    (define-key map [(mouse-3)]                   'maplev-mint-right-click)
    (define-key map [(control c) (control c)]     'maplev-mint-handler)
    map)
  "Keymap used in Mint mode.")

;;}}}
;;{{{ menu

(easy-menu-define maplev-mint-mode-menu maplev-mint-mode-map
  "Menu for Mint buffer."
  '("Mint"
    ["Fix errors" maplev-mint-fix-errors :visible nil] ; not yet defined
    ["Rerun mint" maplev-mint-rerun t]
    ["Quit"       quit-window t]))

;;}}}
;;{{{ mode definition

(define-derived-mode maplev-mint-mode fundamental-mode
  "Major mode for displaying Mint output.
\\{maplev-mint-mode-map}"
  :syntax-table maplev-mint-mode-syntax-table
  :abbrev-table nil
  (setq mode-name "Mint"
	buffer-read-only t)
  (make-local-variable 'maplev-mint--code-buffer)
  (set (make-local-variable 'paragraph-start) "[^ ]")
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (maplev-mint-fontify-buffer)
  (setq truncate-lines nil))

(defun maplev-mint-setup (code-buffer config)
  "Unless already assigned, set `major-mode' to `maplev-mint-mode'.
Set `maplev-mint--code-buffer' to CODE-BUFFER, the buffer that
contains the source code.  Set buffer-local variable
`maplev-config' to CONFIG."
  (unless (eq major-mode 'maple-mint-mode)
    (maplev-mint-mode))
  (setq maplev-config config
	maplev-mint--code-buffer code-buffer))

;;}}}
;;{{{ mode functions

(defun maplev-mint--goto-source-pos (line char &optional file)
  "Move to LINE, then forward CHAR characters, in FILE and return position.
If FILE is nil, use buffer `maplev-mint--code-buffer'.
Pop up the buffer, move to either `point-min', if FILE is non-nil,
or `maplev-mint--code-beginning' otherwise,
and move forward LINE lines and CHAR columns."
  (switch-to-buffer-other-window (if file (find-file-noselect file)
				   maplev-mint--code-buffer))
  (goto-char (if file (point-min)  maplev-mint--code-beginning))
  (if (> line 0) (forward-line line))
  (forward-char char)
  (point))

(defun maplev-mint--goto-error (pos)
  "Go to error in Maple source according to Mint message at position POS.
Return position of error in Maple source."
  (let (line col)
    (save-excursion
      (goto-char pos)
      ;; The location of the error is indicated by the caret
      ;; in the Mint output.
      (when (search-backward "^" (line-beginning-position) t)
        (setq col (current-column))
        (forward-line -1)
        (re-search-forward  "[0-9]+")
        (setq line (1- (string-to-number (match-string 0)))
              col  (- col (current-column) 2))))
    (maplev-mint--goto-source-pos line col)))

(defun maplev-mint--goto-source-proc-old (pos)
  "According to Mint buffer position POS, move point to the end of the
initial assignment statement of a source procedure/module.  This would
be either the closing parenthesis of the formal parameter list, or the
terminating semicolon or colon of an optional procedure/module type
declaration.  Return non-nil if this is a procedure, nil if an operator.

THIS NEEDS WORK TO HANDLE OPERATORS."
  ;; This function uses a fairly complicated regexp in an attempt to
  ;; match the appropriate procedure assignment.  In one sense this is
  ;; overkill; Mint indicates the line number of the start of the
  ;; procedure, so we should be able to go directly to the procedure on
  ;; that line.  It is possible, however, to have a nested procedure on
  ;; the same line as another procedure.  More to the point, a nested
  ;; anonymous procedure inside an anonymous procedure.  In that case the
  ;; only distinction is the argument list.  Does this happen enough to
  ;; justify this code?   If we merely desire to move point to the
  ;; correct place in the source, getting to the right line is
  ;; sufficient.  But if there is some automated work to do, the exact
  ;; point is required.  One way to avoid this complexity is to not
  ;; offer the user the option of automatically adding or deleting
  ;; variables from an anonymous procedure.  The sticking point is that
  ;; Mint, alas, considers indexed names to be anonymous procedures so
  ;; their frequency is greater than should be.

  (let (name-re args-re line case-fold-search)
    (save-excursion
      (goto-char pos)
      (re-search-backward "^\\(Nested \\)?\\(Anonymous \\)?\\(Procedure\\|Operator\\|Module\\)")
      ;; Assign name-re the procedure/module name.
      (setq name-re (if (nth 4 (match-data)) ; t if anonymous procedure
                        ""
                      (save-excursion
                        ;; Use `(' to terminate proc-name
                        ;; (re-search-forward "\\(Procedure\\|Module\\)[ \t]*\\([^(]*\\)")
                        (re-search-forward "\\(Procedure\\|Module\\)\\s-*\\([^[(]*\\)")
                        (concat "`?" (match-string-no-properties 2)
                                "\\([ \t\f\n]*\\[[^]]*\\]\\)*" ; optional indices
                                "[ \t\n]*:=[ \t\n]*")))
            ;; Assign a regular expression that matches the argument
            ;; list in the source.  The generated regexp does not
            ;; match an argument list with duplicate arguments; this
            ;; because Mint does not print the duplicate arguments.
            ;; This can be improved, made more robust.
            ;; Allow comments before commas, too.
            args-re (save-excursion
                      (re-search-forward "(\\([^)]*\\))")
                      (maplev--replace-string
                       (match-string-no-properties 1)
                       `(("::" . " :: ")
                         ("[ \t\n]+" . "[ \t\n]*")
                         ("," . ,(concat "\\([ \t]*\\(#.*\\)?\n\\)*[ \t]*"
                                         ","
                                         "\\([ \t]*\\(#.*\\)?\n\\)*[ \t]*")))))
            ;; Assign a regular expression that matches any argument
            ;; list.  This may be tougher than I envisioned.  How are
            ;; optional type declarations handled?  The difficulty is
            ;; that they could have commas and closing parentheses.
            
            ;;            args-re (concat "\\s-*\\<\\w+\\>\\(\\s-*::\\s-*[^
            )
      (re-search-forward "on\\s-*lines?\\s-*\\([0-9]+\\)")
      (setq line (1- (string-to-number (match-string 1)))))
    
    ;; move point in source to beginning of line where procedure/module assignment begins.

    (maplev-mint--goto-source-pos line 0)

    ;; move forward to end of assignment.

    (unless (re-search-forward (concat name-re
                                       "\\(proc\\|module\\)[ \t\n*]*"
                                       "(\\([ \t]*\\(#.*\\)?\n\\)*"
                                       args-re
                                       "\\([ \t\n]*#.*$\\)*[ \t\n]*)"
                                       "\\(\\s-*::\\s-*\\<\\w+\\>\\s-*[;:]\\)?" ; optional procedure type
                                       )
                               nil t)
      ;; If search failed (possibly because of duplicate arguments,
      ;; try again without explicitly specifying the argument list.
      (goto-char (maplev--scan-lists 1)))))

(defun maplev-mint--goto-include-file (pos)
  "Open include file corresponding to link at POS in mint buffer."
  (goto-char pos)
  (when (search-backward "included: ")
    (goto-char (match-end 0))
    (when (looking-at ".*$")
      (let ((file (maplev-find-include-file
		   (match-string-no-properties 0)
		   (slot-value maplev-config 'include-path)
		   'inc-first)))
	(when file
	  (find-file-other-window file))))))
  

(defun maplev-mint--goto-source-proc (pos)
  "Move to position in source buffer corresponding to link at POS in mint buffer.
This position is after the formal parameter list of the operator,
procedure, or module."
  (maplev-mint--goto-source-and-get-region pos))

(defun maplev-mint--goto-source-and-get-region (pos)
  "Move to the source buffer and return a cons cell of the region
in the source buffer of the proc/module referenced at POS in the
mint buffer.  Point in the source buffer is set to just after the
formal parameter list."
  (let (beg class end file (line 0) toline)
    (save-excursion
      (goto-char pos)
      (re-search-backward "^\\(Nested \\)?\\(Anonymous \\)?\\(Procedure\\|Operator\\|Module\\)" nil 'noerror)
      ;; Assign class Procedure, Operator, or Module
      (setq class (match-string-no-properties 3))
      (when (re-search-forward "on\\s-*lines?\\s-*\\([0-9]+\\)" nil 'noerror)
	(setq line (1- (string-to-number (match-string-no-properties 1)))
	      file (maplev-mint-get-source-file)
	      toline (if (re-search-forward "to\\s-*\\([0-9]+\\)" (line-end-position 2) t)
			 (string-to-number (match-string-no-properties 1))
		       (1+ line)))))
    ;; move point to the beginning of that line in the source
    (maplev-mint--goto-source-pos line 0 file)
    (setq beg (point))
    (setq end (if (null toline)
		  (point-max)
		(save-excursion
		  (forward-line (- toline line))
		  (point))))
    ;; Use class to position point after formal parameter list
    (cond
     ((string= class "Procedure")
      (when (re-search-forward "\\<proc *(" (line-end-position) t)
	(backward-char)
	(goto-char (maplev--scan-lists 1))))
     ((string= class "Module")
      (when (re-search-forward "\\<module *(" (line-end-position) t)
	(backward-char)
	(goto-char (maplev--scan-lists 1))))
     ((string= class "Operator")
      (when (re-search-forward " *->" (line-end-position) t)
	(goto-char (match-beginning 0)))))
    (cons beg end)))

(defun maplev-mint--goto-source-line (pos)
  "Goto the location in source specified by the line number in the Mint buffer.
The line number begins at character position POS."
  (goto-char pos)
  (beginning-of-line)
  (let ((is_nested (looking-at "  ")))
    (re-search-forward "line +\\([0-9]+\\)" (line-end-position))
    (let ((line (1- (string-to-number (match-string 1)))))
    (maplev-mint--goto-source-pos
     line 0
     (and
       (re-search-backward "^\\(Nested \\)?\\(Anonymous \\)?\\(Procedure\\|Operator\\|Module\\)" nil t)
       (re-search-forward "on\\s-*lines?\\s-*\\([0-9]+\\)" nil t)
       (maplev-mint-get-source-file))))))

(defun maplev-mint-get-source-file ()
  "Return the absolute path to the source file starting at current position."
  (when (looking-at "\\s-+\\(to\\s-+[0-9]+\\s-+\\)?of\\s-+\\(.*\\)")
    (let ((file (maplev-expand->file-name (match-string-no-properties 2) (slot-value maplev-config 'mapledir))))
      (maplev-find-include-file file (slot-value maplev-config 'include-path) 'inc-first))))

(defun maplev--replace-string (string replace)
  "In STRING replace as specified by REPLACE.
REPLACE is an alist with elements \(OLD . NEW\)."
  (while replace
    (let ((pos 0)
          (old (caar replace))
          (new (cdar replace)))
      (while (and (< pos (length string))
                  (setq pos (string-match old string pos)))
        (setq string (replace-match new t t string)
              pos (+ pos (length new)))))
    (setq replace (cdr replace)))
  string)

;;}}}
;;{{{ fontify

(defcustom maplev-mint-proc-face 'font-lock-function-name-face
  "Face name for procedure names in a Mint buffer."
  :type 'face
  :group 'maplev-faces
  :group 'maplev-mint)

(defcustom maplev-mint-warning-face 'font-lock-warning-face
  "Face name for warnings in a Mint buffer."
  :type 'face
  :group 'maplev-faces
  :group 'maplev-mint)

(defcustom maplev-mint-error-face 'font-lock-warning-face
  "Face name for error messages in a Mint buffer."
  :type 'face
  :group 'maplev-faces
  :group 'maplev-mint)

(defcustom maplev-mint-note-face 'font-lock-warning-face
  "Face name for notes in a Mint buffer."
  :type 'face
  :group 'maplev-faces
  :group 'maplev-mint)

(defcustom maplev-mint-link-face 'link
  "Face name for links in a Mint buffer."
  :type 'face
  :group 'maplev-faces
  :group 'maplev-mint)

(defconst maplev-mint-variables-re
  "[ \t\n]*\\(\\(.*,[ \t]*\n\\)*.*\\)[ \t]*$"
  "Regexp used to match the argument list of procedures in Mint output.")

(defconst maplev-mint-fontify-alist
  '(("^\\(on line[ \t]*[0-9]+:\\)" maplev-mint-note-face)
    ("^[ \t]*\\(\\^.*$\\)" maplev-mint-error-face 'error)
    ("^\\(?:Nested \\)?\\(?:Procedure\\|Operator\\|Module\\)[ ]*\\([^(]*\\)" maplev-mint-link-face 'proc)
    ("^\\(?:Nested \\)?Anonymous \\(?:Procedure\\|Operator\\|Module\\)[ ]*\\(\\(?:proc\\|module\\) *([^)]*)\\)" maplev-mint-link-face 'proc)
    ("^This source file is included: \\(.*\\)" maplev-mint-link-face 'include-file)
    ("These exported variables were never used:" maplev-mint-warning-face 'unused-export t)
    ("These global variables were declared, but never used:" maplev-mint-warning-face 'unused-global t)
    ("These local variables were never used:" maplev-mint-warning-face 'unused-local t)
    ("These local variables were not declared explicitly:" maplev-mint-warning-face 'undecl-local t)
    ("These names appeared more than once in the parameter list:" maplev-mint-warning-face 'repeat-arg t)
    ("These names were declared as both a local variable and a parameter:" maplev-mint-warning-face 'local-and-param t)
    ("These names were declared more than once as a local variable:" maplev-mint-warning-face 'repeat-local t)
    ("These names were used as global names but the names don't start with _:" maplev-mint-warning-face 'undecl-global t)
    ("These names were used as global names but were not declared:" maplev-mint-warning-face 'undecl-global t)
    ("These parameters were never used\\(?: explicitly\\)?:" maplev-mint-warning-face 'unused-arg t)
    ;;("These local variables were assigned a value, but otherwise unused:" ... )
    ("\\(on line +[0-9]+\\)" maplev-mint-link-face 'goto-line)
   ; ("\\(on lines +[0-9]+\\s-+to\\s-++[0-9]+\\s-+of\\s-+.*\\)" maplev-mint-note-face 'goto-line)
    )
  "Alist for fontification in a Mint buffer.
Each element is a list of the form \(REGEXP FACE PROP VAR\):
- REGEXP is to be matched;
- FACE is a face applied to the first regexp group;
- PROP is a symbol applied as a text property to the first regexp group.
- VAR is optional, if non-nil REGEXP is catenated with `maplev-mint-variables-re';
  doing so causes the following variables to be in a regexp group.")

(defun maplev-mint-fontify-buffer ()
  "Fontify the mint buffer.  Does not use font-lock mode."
  (save-excursion
    (let ((mlist maplev-mint-fontify-alist)
          regexp mel buffer-read-only case-fold-search)
      (font-lock-mode -1) ; turn-off font-lock
      ;; Process elements of maplev-mint-fontify-alist
      (while (setq mel (car mlist))
	(goto-char (point-min))
	(setq regexp (concat (nth 0 mel)
                             (if (nth 3 mel) maplev-mint-variables-re)))
	(while (re-search-forward regexp nil t)
          (let ((beg (match-beginning 1))
		(end (match-end 1)))
            ;; Here we are working with variables whose values are symbols
            ;; with a face property.
	    (let ((prop (nth 1 mel)))
	      (put-text-property beg end 'face (eval prop))
	      (when (eq prop 'maplev-mint-link-face)
		(put-text-property beg end 'mouse-face 'highlight)
		(put-text-property beg end 'help-echo "goto source")))
            (when (nth 2 mel)
              ;; Embed mint action as value of text property `maplev-mint'
              (put-text-property beg end 'maplev-mint (eval (nth 2 mel)))
              (if (nth 3 mel)
                  (save-excursion
                    (goto-char beg)
                    (while (re-search-forward "\\_<\\(\\w+\\)\\_>\\(::[^ \n]+\\)?\\( := [^ \n]*\\)?" end t)
                      (put-text-property (match-beginning 1) (match-end 1)
					 'mouse-face 'highlight)))))))
	(setq mlist (cdr mlist)))
      (set-buffer-modified-p nil))))

;;}}}
;;{{{ interactive functions

(defun maplev-mint-query (form &rest vars)
  "Return t if correction suggested by mint should be made.
FORM and VARS are used for `y-or-n-p' query.  Return t if
`maplev-mint-query-flag' is nil."
  (or (not maplev-mint-query-flag)
      (y-or-n-p (apply 'format form vars))))

(defun maplev-mint-click (click)
  "Move point to CLICK."
  (interactive "e")
  (set-buffer (window-buffer (select-window (event-window click))))
  (maplev-mint-handler (event-point click)))

(defun maplev-mint-right-click (click)
  "Move point to CLICK."
  (interactive "e")
  (set-buffer (window-buffer (select-window (event-window click))))
  (maplev-mint-handler (event-point click) t))

(defun maplev-unpack-list-of-one (lst)
  "Return the contents of LST if it has one element, otherwise return LST."
  (if (= 1 (length lst))
      (car lst)
    lst))

(defun maplev-mint-handler (pos &optional all-vars)
  "Handle mint output at position POS.
When called interactively, POS is position where point is.
ALL-VARS non-nil means handle all variables, not just the one clicked on."
  (interactive "d")
  (let ((prop (get-text-property pos 'maplev-mint))
	(code-buffer maplev-mint--code-buffer))
    (when prop
      (let* ((vars (if all-vars
		      (split-string (buffer-substring-no-properties
				     (next-single-property-change pos 'maplev-mint)
				     (previous-single-property-change (1+ pos) 'maplev-mint))
				    ",[ \t\n]+" t " +")
		    (list (save-excursion
			    (goto-char pos)
			    (maplev--ident-around-point)))))
	     (arg (if (= 1 (length vars)) (car vars) vars))
	     (cont t))
	(cond
	 ;; Jump to an included file
	 ((eq prop 'include-file)
	  (maplev-mint--goto-include-file pos))
	 ;;
	 ;; Jump to the start of a procedure in the source.
	 ((eq prop 'proc)
	  (maplev-mint--goto-source-proc pos))
	 ;;
	 ;; Jump to the location of an error in the source code.
	 ((eq prop 'error)
	  (maplev-mint--goto-error pos))
	 ;;
	 ;; Remove unused args from argument list.
	 ((eq prop 'unused-arg)
	  (when (maplev-mint-query "Delete `%s' from argument list? " arg)
	    (maplev-mint--goto-source-proc pos)
	    (maplev-delete-vars vars (maplev--scan-lists -1) (point))))
	 ;;
	 ;; Remove unused local variables from local declaration.
	 ((eq prop 'unused-local)
	  (when (maplev-mint-query "Delete `%s' from local statements? " arg)
	    (maplev-delete-declarations "local" vars (maplev-mint--goto-source-and-get-region pos))))
	 ;;
	 ;; Remove unused exported variables from export declaration.
	 ((eq prop 'unused-export)
	  (when (maplev-mint-query "Delete `%s' from export statements? " arg)
	    (maplev-delete-declarations "export" vars (maplev-mint--goto-source-and-get-region pos))))
	 ;;
	 ;; Remove unused global variables from global declaration.
	 ((eq prop 'unused-global)
	  (when (maplev-mint-query "Delete `%s' from global statements? " arg)
	    (maplev-delete-declarations "global" vars (maplev-mint--goto-source-and-get-region pos))))
	 ;;
	 ;; Remove repeated args from argument list.
	 ((eq prop 'repeat-arg)
	  (when (maplev-mint-query "Remove duplicates of `%s' from parameters? " arg)
	    (maplev-mint--goto-source-proc pos)
	    (maplev-delete-vars vars (maplev--scan-lists -1) (point) 1)))
	 ;;
	 ;; Remove repeated local variables from local declaration.
	 ((eq prop 'repeat-local)
	  (when (maplev-mint-query "Remove duplicates of `%s' from local statements? " arg)
	    (maplev-delete-declarations "local" vars (maplev-mint--goto-source-and-get-region pos))
	    (maplev-add-declaration "local" vars)))
	 ;;
	 ;; Declaration of undeclared locals variables.
	 ((eq prop 'undecl-local)
	  (let ((action (x-popup-dialog t `(,(if (stringp arg)
						 (format "Undeclared variable: %s" arg)
					       "All undeclared variables")
					    ("Declare as local" . "local")
					    ("Declare as export" . "export")))))
	    (maplev-mint--goto-source-proc pos)
	    (maplev-add-declaration action vars)))
	 ;;
	 ;; Declaration of undeclared global variables.
	 ((eq prop 'undecl-global)
	  (let (dofile region)
	    (if (save-excursion
		  (goto-char pos)
		  (re-search-backward "These names were used as global names but \\(the names don't start with _\\|were not declared\\)")
		  (beginning-of-line)
		  (looking-at "  "))
		(setq region (maplev-mint--goto-source-and-get-region pos))
	      ; non-indented mint statement; query throughout file
	      (maplev-mint--goto-source-pos 1 0 nil)
	      (setq dofile t
		    region (cons (point-min) (point-max))))
	    (maplev-mint-query-undeclared-globals vars (car region) (cdr region) dofile)))
	 ;;
	 ;; Declared as both local variable and parameter.
	 ((eq prop 'local-and-param)
	  (let ((action (x-popup-dialog t `(,(format "Declared as local and parameter: %s" arg)
					    ("Remove local declaration" . "local")
					    ("Remove parameter" . "param")))))
	    (if (string= action "local")
		(maplev-delete-declarations "local" vars (maplev-mint--goto-source-and-get-region pos))
	      (maplev-mint--goto-source-proc pos)
	      (maplev-delete-vars vars (maplev--scan-lists -1) (point)))))
	 ;;
	 ;; Goto line
	 ((eq prop 'goto-line)
	  (setq cont nil)
	  (maplev-mint--goto-source-line pos)))

	;; rerun mint
	(when (and maplev-mint-rerun-flag cont)
	  (unless maplev-mint--code-buffer
	    (and maplev-mint-save-rerun-flag
		 (buffer-modified-p)
		 ;;(y-or-n-p "save buffer ")
		 (save-buffer)))
	  (set-buffer code-buffer)
	  (maplev-mint-rerun))))))

;;}}}
;;{{{ regions

(defun maplev-mint-region (beg end &optional syntax-only)
  "Run Mint on the current region, from BEG to END.
Return exit code of mint."
  (interactive "r")
  (let ((code-buffer (current-buffer))
        (code-window (get-buffer-window (current-buffer)))
        (coding-system-for-read 'undecided-unix)
        (mint-buffer "*Mint*")
	(config maplev-config)
        status eoi lines errpos)
    ;; Allocate markers, unless they exist
    (unless maplev-mint--code-beginning
      (setq maplev-mint--code-beginning (make-marker)
            maplev-mint--code-end (make-marker)))
    (set-marker maplev-mint--code-beginning beg)
    (set-marker maplev-mint--code-end end)
    (with-current-buffer (get-buffer-create mint-buffer)
      (setq buffer-read-only nil))
    (copy-to-buffer mint-buffer beg end)
    (with-current-buffer mint-buffer
      (goto-char (point-max))
      ;; Add a blank line to the end of the buffer, unless there is
      ;; one already.  This is needed for mint to work properly.
      ;; (That's why mint-buffer is used as a temp buffer for mint input.)
      (if (not (bolp)) (newline))
      ;; remember end-of-input
      (setq eoi (point-max))
      ;; Run Mint
      ;; To get this to work on Windows, the mint options passed to
      ;; call-process-region must be a sequence of strings, one string
      ;; for each option (including an option argument).  Linux works
      ;; with that format, or with the options catenated into a single string
      ;; (separated by spaces, of course).  So am using the format that
      ;; works on both platforms.
      
      (let ((mint (slot-value config 'mint))
	    ;; N.B. mint occasionally generates nonsense output when screen width (-w) is large.
	    (mint-args (append (maplev-get-option-with-include config 'mint-options))) ;;  "-w5000")))
	    (process-environment (if maplev-use-new-language-features
				     (cons "MAPLE_NEW_LANGUAGE_FEATURES=1" process-environment)
				   process-environment)))
	(unless mint
	  (error "The slot-value of :mint in maplev-config is not assigned"))
	(when (and syntax-only (not (member "-S" mint-args)))
	  (setq mint-args (cons "-S" mint-args)))
	(unless (member "-q" mint-args)
	  (setq mint-args (cons "-q" mint-args)))
	(setq status (apply #'call-process-region
			      (point-min) (point-max)
			      mint
			      nil ; do not delete
			      mint-buffer
			      nil  ; do not redisplay
			      mint-args)))
      (delete-region (point-min) eoi)
      ;; Display Mint output
      (maplev-mint-setup code-buffer config)
      (setq lines (if (= (buffer-size) 0)
                      0
                    (count-lines (point-min) (point-max))))
      (cond ((= lines 0)
             ;; Assume: no mint output means no "real" error.
             ;; This happens when the mint info level is 1.
             (setq status 0)
	     (message "no mint warnings"))
            ((= lines 1)
             (goto-char (point-min))
             (message "%s" (buffer-substring-no-properties
                            (point) (line-end-position))))
            ((> lines 1)
             (display-buffer (current-buffer))))
      ;; If error in maple source (should be identical to status > 0)
      ;; locate position of error
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*\\^" nil t)
          (setq errpos (maplev-mint--goto-error (point)))))
	
    ;; If there is an error in the maple source and a window displays it,
    ;; move point in this window
    (when (and code-window errpos)
      (set-window-point code-window errpos)
      (switch-to-buffer mint-buffer))
    status))

(defun maplev-mint-buffer ()
  "Run Mint on the current buffer."
  (interactive)
  (widen)
  (maplev-mint-region (point-min) (point-max)))

(defun maplev-mint-procedure ()
  "Run Mint on the current procedure."
  (interactive)
  (apply 'maplev-mint-region (maplev-current-defun)))

(defun maplev-mint-rerun ()
  "Rerun Mint on the previously executed region.
If no region has been selected, run Mint on the buffer."
  (interactive)
  (save-current-buffer
    (if maplev-mint--code-buffer        ; we are in mint buffer
        (set-buffer maplev-mint--code-buffer))
    (if (not maplev-mint--code-beginning)
        (maplev-mint-buffer)
      (maplev-mint-region (marker-position maplev-mint--code-beginning)
                          (marker-position maplev-mint--code-end)))))

;;}}}
;;{{{ other functions

;;; stuff used by mint

(defun maplev--re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for regular expression REGEXP.
This function is like `re-search-forward', but strings and comments are ignored.
Optional arguments BOUND, NOERROR, and COUNT have the same meaning
as in `re-search-forward'."
  ;; This approach gets confused by a comment inside the match
  ;; (e.g., when REGEXP can match more than one line).
  ;; Therefore it's better to break complex REGEXP's apart
  ;; and handle the items seperately.
  (if (not count) (setq count 1))
  (let ((dir (if (< count 0) -1 1))
        (pos (point))
	ppsexp
        case-fold-search)
    (with-syntax-table maplev-quote-not-string-syntax-table
      (while (and (not (zerop count)) pos)
	(setq pos (re-search-forward regexp bound noerror dir))
	(while (progn
		 (setq ppsexp (parse-partial-sexp (maplev-safe-position) (point)))
		 (and (or (nth 3 ppsexp) (nth 4 ppsexp))
		      (setq pos (re-search-forward regexp bound noerror dir)))))
	(setq count (- count dir)))
      pos)))
      
(defun maplev--re-search-backward (regexp &optional bound noerror count)
  "Search backward from point for regular expression REGEXP.
This function is like `re-search-backward', but strings and comments are ignored.
Optional arguments BOUND, NOERROR, and COUNT have the same meaning
as in `re-search-backward'."
  ;; See maplev--re-search-forward.
  (if (not count) (setq count 1))
  (let ((dir (if (< count 0) -1 1))
        (pos (point))
	ppsexp
        case-fold-search)
    (with-syntax-table maplev-quote-not-string-syntax-table
      (while (and (not (zerop count)) pos)
	(setq pos (re-search-backward regexp bound noerror dir))
	(while (progn
		 (setq ppsexp (parse-partial-sexp (maplev-safe-position) (point)))
		 (and (or (nth 3 ppsexp) (nth 4 ppsexp))
		      (setq pos (re-search-backward regexp bound noerror dir)))))
	(setq count (- count dir)))
      pos)))

(defun maplev-safe-position (&optional to)
  "Search for safe buffer position before point \(a position not in a comment\).
Optional arg TO initializes the search.  It defaults to point.
FIXME.  THIS IS NOT ROBUST."
  (unless to (setq to (point)))
  (save-excursion
    (save-match-data
      (goto-char to)
      (while (and (= 0 (forward-line -1))
                  (looking-at "#")))
      (point))))

(defun maplev--scan-lists (count &optional from)
  "Scan COUNT lists.  Optional arg FROM defaults to position of point.
Returns the character number of the position thus found."
  (if (not from) (setq from (point)))
  (let ((parse-sexp-ignore-comments t))
    (scan-lists from count 0)))

(defun maplev-delete-whitespace (&optional back)
  "Delete whitespace characters plus empty comments at point.
If optional arg BACK is non-nil, delete whitespace characters before point."
  ;; It would be nice to have a function looking-at-backward,
  ;; but there is nothing like that. (Guess why :-)
  (if back (let ((pos (point)))
             (skip-chars-backward " \t\n")
             (delete-region pos (point)))
    (save-match-data
      ;; Is this regexp too aggressive?
      (if (looking-at "\\([ \t\n]\\|\\(#[ \t]*$\\)\\)*")
          (delete-region (match-beginning 0) (match-end 0))))))

(defun maplev--statement-terminator ()
  "Return position after the next statement terminator."
  (save-excursion
    (maplev--re-search-forward "[^:]\\(;\\|:[^-:=]\\)" nil t)
    (+ 1 (match-beginning 1))))

(defun maplev--goto-declaration (keyword)
  "Move point to after KEYWORD in the KEYWORD declaration in a Maple procedure.
Return nil if there no such statement.  Point must be to the right of
the closing parenthesis in the formal parameter list."
  (let ((bound (save-excursion
                 (maplev--re-search-forward maplev--defun-re
                                            ;; (maplev-end-of-proc) 'move)
                                            (maplev--end-of-defun-pos) 'move)
                 (point))))
    (if (save-excursion
          (maplev--re-search-forward
           (concat "\\<" keyword "\\>") bound t))
        (goto-char (match-end 0)))))

(defun maplev-mint-forward-declaration-symbol ()
  "Move forward to the start of the next symbol in a declaration statement.
Skip over comments and types."
  (interactive)
  (with-syntax-table maplev-symbol-syntax-table
    (cond
     ((looking-at "`")
      (forward-char)
      (search-forward "`"))
     ((looking-at "%")
      (maplev-forward-expr)))
    (re-search-forward "\\W\\b\\|#\\|(\\*\\|::\\|:=\\|`" nil t)
    (let ((match (match-string-no-properties 0)))
      (cond
       ((equal "#" match)
	(forward-line)
	(maplev-mint-forward-declaration-symbol))
       ((equal "(*" match)
	(search-forward "*)")
	(maplev-mint-forward-declaration-symbol))
       ((or (equal "::" match)
	    (equal ":=" match))
	(maplev-forward-expr)
	(maplev-mint-forward-declaration-symbol))
       ((equal "`" match)
	(backward-char)
	t)
       (t)))))

       
(defun maplev-add-declaration (keyword vars)
  "To the current procedure's KEYWORD declaration add VARS, a list of variables.
If necessary, add a KEYWORD statement.  Point must be after the closing
parenthesis of the procedure's argument list."
  (when vars
    (save-excursion
      (unless (maplev--goto-declaration keyword)
	;; The declaration KEYWORD does not exist; insert it.
	(let (stay)
	  ;; Declarations are ordered: local, global, export
	  (if (maplev--goto-declaration "local")
	      (setq stay (goto-char (maplev--statement-terminator))))
	  (if (maplev--goto-declaration "global")
	      (setq stay (goto-char (maplev--statement-terminator))))
	  ;; Position point and text in preparation for inserting a
	  ;; declaration statement.
	  (if (not (looking-at "[ \t]*\\(#.*\\)?$")) ; More code on line?
	      (just-one-space)      ; Then insert declaration inbetween.
	    (forward-line)            ; Else move to the next code line.
	    (unless stay                 ; Keep moving if we not already
	      (while (looking-at "[ \t]*#") ; have a declaration.
		(forward-line)))))
	;; Insert the declaration statement KEYWORD VAR; at point,
	;; with VAR the first variable.  If point is at beginning of
	;; line, insert a newline at end.  NOTE: It might be better to
	;; look whether there is any following text.
	(let ((new-line (bolp)))
	  (insert keyword " " (car vars) ";")
	  (setq vars (cdr vars))
	  (when new-line
	    (newline)
	    (forward-line -1)))
	(maplev-indent-line)
	(forward-word))

      ;; insert remaining variables

      (let ((start (point)))
	(while vars
	  (goto-char start)
	  (let ((var (car vars))
		(regex (concat "\\s-*\\(" maplev--symbol-re "\\) *,?"))
		(end (maplev--statement-terminator)))
	    (setq vars (cdr vars))
	    (if maplev-alphabetize-declarations-p
		(progn
		  ;; (forward-word) ; skip over keyword
		  (while (and (< (point) end)
			      (maplev-mint-forward-declaration-symbol)
			      (looking-at regex)
			      (string< (match-string-no-properties 1) var))
		    (goto-char (match-end 0)))
		  (if (< end (point))
		      (goto-char (1- end)))
		  (unless (looking-at (concat (regexp-quote var) "\\_>"))
		    (let ((space (make-string maplev-variable-spacing ?\ )))
		      (if (looking-at "\\s-*[:;]")
			  (insert (format ",%s%s" space var))
			(insert (format "%s," var) space)))))
	      ;; non-alphabetic, insert VAR at end.
	      (goto-char end)
	      (backward-char)
	      (insert "," (make-string maplev-variable-spacing ?\ ) var))))))))
	
(defun maplev-add-local-variable (var)
  "Add VAR to the current procedure's local statement.
Interactively, VAR defaults to the identifier at point."
  (interactive (list (maplev-ident-around-point-interactive
                      "Local variable")))
  (maplev-add-variable "local" var))

(defun maplev-add-global-variable (var)
  "Add VAR to the current procedure's local statement.
Interactively, VAR defaults to identifier point is on."
  (interactive (list (maplev-ident-around-point-interactive
                      "Global variable")))
  (maplev-add-variable "global" var))

(defun maplev-add-export-variable (var)
  "Add VAR to the current module's export statement.
Interactively, VAR defaults to identifier point is on."
  (interactive (list (maplev-ident-around-point-interactive
                      "Exported variable")))
  (maplev-add-variable "export" var))

(defun maplev-add-variable (keyword var)
  "Add VAR to the current procedure's KEYWORD declaration."
  (save-excursion
    (maplev-beginning-of-defun)
    (when (re-search-forward ":=\s-*proc" nil t)
      (goto-char (maplev--scan-lists 1))
      (maplev-add-declaration keyword (list var)))))

(defun maplev-add-variable-here (keyword var begin)
  "Add VAR to KEYWORD declaration of procedure/module that begins at BEGIN."
  (save-excursion
    (goto-char begin)
    (unless (re-search-forward "\\<proc\\|module\\>" nil t)
      (error "Cannot find start of procedure/module"))
    ;; skip over argument sequence
    (goto-char (maplev--scan-lists 1))
    ;; skip an optional return type
    (when (looking-at "\\s-*::")
      (maplev-forward-expr)
      (if (looking-at "\\s-*[:;]")
	  (goto-char (match-end 0))
	(error "Cannot find end of return type")))
    (maplev-add-declaration keyword (list var))))

(defun maplev-delete-declarations (keyword vars region)
  "Delete VARS from KEYWORD declarations in the specified REGION."
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (cdr region))
      (let ((regex (concat "\\(\\<" keyword "\\>\\)"
			   "\\|\\(" maplev--defun-re "\\)"
			   "\\|\\(?:" maplev--defun-end-re "\\)"))
	    (cnt 0) ; keep track whether in original procedure
	    term)
	(while (maplev--re-search-forward regex nil 'move)
	  (if (match-string 1)
	      (when (zerop cnt)
		(let ((beg (match-beginning 0)))
		  (maplev-delete-vars vars (point) (maplev--statement-terminator))
		  ;; remove entire KEYWORD statement, if empty
		  (save-excursion
		    (goto-char beg) ; move before KEYWORD
		    (when (looking-at (concat keyword "[ \t\n]*[;:]\\([ \t#]*$\\)?"))
		      (delete-region (match-beginning 0) (match-end 0))
		      (maplev-delete-whitespace t)))))
	    ;; adjust cnt up/down when entering/exiting a proc/module.
	    (setq cnt (+ cnt (if (match-string 2) +1 -1)))))))))

(defun maplev-delete-vars (vars start end &optional leave-one)
  "Delete VARS in region between START and END; VARS must be
either a string or a list of strings.  If optional argument
LEAVE-ONE is non-nil, the first occurrence of VARS is not
deleted."
  (let ((parse-sexp-ignore-comments)
        case-fold-search lo var)
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (if (stringp vars) (setq vars (list vars)))
        (while vars
          (setq lo leave-one)
	  (setq var (car vars))
          (goto-char (point-min))
          (while (maplev--re-search-forward
		  ;; hack to handle backquoted symbols
		  (if (= (aref var 0) ?`)
		      (regexp-quote var)
		    (concat "\\_<" (regexp-quote var) "\\_>"))
                  nil t)
            (if lo
                (setq lo nil)
	      (delete-region (match-beginning 0)
			     (progn
			       (maplev-delete-whitespace)
			       (maplev-forward-expr)
			       (point)))
              ;; Remove separating comma
              (when (or (maplev--re-search-backward "," nil t)
                        (maplev--re-search-forward  "," nil t))
                (delete-region (match-beginning 0) (match-end 0))
                (maplev-delete-whitespace))))
          (setq vars (cdr vars)))))))

(defconst maplev-mint-query-help
  " y   single-quote the match
 '   single-quote the match
 !   single-quote remaining matches
 Q   single-quote remaining matches
 \"  double-quote the match
 n   skip to next match
 d   skip all occurrences of current match
 e   edit the list of variables
 g   declare match as a global variable
 l   declare match as a local variable
 :   convert [foo] to :-foo
C-l  recenter the screen
C-r  enter recursive edit (C-M-c to get out)
 q   quit
 h   display this help
 f1  display this help"
  "Help message while in `maple-mint-quote-vars'.")

(defvar maplev-mint-query-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'single-quote)  ; maybe this should be repeat last?
    (define-key map "y" 'single-quote)
    (define-key map "'" 'single-quote)
    (define-key map "Q" 'single-quote-rest)
    (define-key map "!" 'single-quote-rest)

    (define-key map "\"" 'double-quote)

    (define-key map "d" 'delete)
    (define-key map "e" 'edit-vars)

    (define-key map "g" 'global)
    (define-key map "G" 'global-rest)
    (define-key map "l" 'local)
    (define-key map "L" 'local-rest)

    
    (define-key map "n" 'skip)
    (define-key map "N" 'skip)
    (define-key map "\d" 'skip)
    (define-key map "[delete]" 'skip)

    (define-key map ":" 'colon-dash)

    (define-key map "q" 'exit)
    (define-key map "\r" 'exit)
    (define-key map "[return]" 'exit)

    (define-key map "C-g" 'quit)
    (define-key map "C-]" 'quit)

    (define-key map "\C-l" 'recenter)
    (define-key map "\C-r" 'edit)

    (define-key map "h"    'help)
    (define-key map "[f1]" 'help)
    (define-key map [help] 'help)
    map))

(defun maplev-mint-query-undeclared-globals (vars beg end dofile)
  "Query to handle occurrences of VARs in the region between BEG and END.
When DOFILE is non-nil, ...
VARs is a list of undeclared globals."
  (let (case-fold-search regexp reply start var)
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(let ((syntax-table maplev-quote-not-string-syntax-table)
	      (cnt (if dofile 0 -1))
	      (message (apply 'propertize
			      (substitute-command-keys
			       "Query replacing %s with %s: (\\<maplev-mint-query-map>\\[help] for help) ")
			      minibuffer-prompt-properties))
	      (keep-going t)
	      def done double-quoted key match quoted quote-rest real-match-data regex unquoted)
	  (goto-char beg)
	  (unwind-protect
	      (while keep-going
		(unless regex
		  ;; assign regex. It has the following numbered groups:
		  ;; 1) :-
		  ;; 2) variable
		  ;; 3) :-
		  ;; 4) proc|module|->
		  ;; 5) end (proc|module)?

		  (setq regex (concat "\\(:-\\)?\\_<`?\\(" (regexp-opt vars) "\\)`?\\_>\\(:-\\)?"
				      "\\|\\(" maplev--defun-re "\\|->\\)"
				      "\\|\\(" maplev--defun-end-re "\\)")))
		(if (not (maplev--re-search-forward regex nil 'move))
		    (setq keep-going nil)
		  (setq real-match-data (match-data 'integers))
		  (setq done nil)
		  (while (not done)
		    (cond
		     ((match-string-no-properties 5) ; typically `module`
		      (maplev-forward-expr)
		      (setq done t))
		     ((or (match-string-no-properties 1) (match-string-no-properties 3))
		      (setq done t)) ; skip :- fields (left or right)
		     ((match-string-no-properties 2)
		      ;; matched a variable
		      (if (not (zerop cnt))
			  ;; skip matches inside a proc/module
			  (setq done t)
			(setq match (match-string-no-properties 0)
			      unquoted (match-string-no-properties 2)
			      quoted  (concat "'" unquoted "'")
			      double-quoted  (concat "\"" unquoted "\"")
			      start (match-beginning 0))
			(if (looking-at "'") ; FIXME: this fails if the match is part of a protected expression
			    ;; skip protected matches
			    (setq done t)
			  (if quote-rest
			      ;; quote the match and continue
			      (progn
				(replace-match quoted)
				(setq done t))
			    (replace-highlight (match-beginning 0) (match-end 0) nil nil (concat "\\_<" match "\\_>") 'regexp nil nil nil)
			    (message message match quoted)
			    (setq key (vector (read-event)))
			    (setq def (lookup-key maplev-mint-query-map key))
			    (cond
			     ((eq def 'help)
			      (with-output-to-temp-buffer "*Help*"
				(princ
				 (concat "Query replacing "
					 match
					 " with "
					 quoted
					 ".\n\n"
					 (substitute-command-keys maplev-mint-query-help)))
				(with-current-buffer standard-output
				  (help-mode)))
			      (set-match-data real-match-data))
			     ((eq def 'single-quote)
			      (replace-match quoted)
			      (setq done t))
			     ((eq def 'double-quote)
			      (replace-match double-quoted)
			      (setq done t))
			     ((or (eq def 'global)
				  (eq def 'local))
			      ;; add variable to local/global declaration
			      (maplev-add-variable-here (symbol-name def) match beg)
			      ;; remove match from vars and update regex
			      (setq vars (delete match vars)
				    regex nil
				    done t))
			     ((or (eq def 'global-rest)
				  (eq def 'local-rest))
			      (let ((var match)
				    (keyword (if (eq def 'global-rest) "global" "local")))
				(while vars
				  (maplev-add-variable-here keyword var beg)
				  (setq var (car vars)
					vars (cdr vars))))
			      (setq done t
				    keep-going nil))
			     ((eq def 'delete)
			      ;; remove match from vars and update regex
			      (setq vars (delete match vars)
				    regex nil
				    done t))
			     ((eq def 'skip)
			      ;; skip to next match
			      (setq done t))
			     ((eq def 'exit)
			      ;; exit
			      (setq keep-going nil done t))
			     ((eq def 'edit)
			      ;; recursively edit at current match
			      (let ((opos (point-marker)))
				(goto-char (match-beginning 0))
				(save-excursion
				  (save-window-excursion
				    (recursive-edit)))
				(goto-char opos)
				(set-marker opos nil))
			      (set-match-data real-match-data))
			     ((eq def 'edit-vars)
			      ;; edit the list of vars
			      (setq vars (maplev-mint-edit-vars vars)
				    regex nil))
			     ((eq def 'single-quote-rest)
			      ;; quote current and remaining vars
			      (setq quote-rest t))
			     ((eq def 'colon-dash)
			      ;; convert match to colon-dash
			      (save-excursion
				(goto-char (1- start))
				(when (looking-at (concat "\\[" match "\\]"))
				  (replace-match (concat ":-" unquoted))))
			      (setq done t))
			     ((eq def 'recenter)
			      ;; recenter screen
			      (recenter-top-bottom))
			     (t
			      (setq this-command 'mode-exited)
			      (setq done t))
			     )))))
		     (t (let ((keyword (match-string-no-properties 4)))
			  (if (string= keyword "->")
			      (maplev-forward-expr)
			    (setq cnt (+ cnt (if keyword +1 -1)))))
			(setq done t))))))
	    (replace-dehighlight)))))))

;;}}}


(defvar maplev-mint-var-mode-map
  (let ((map tabulated-list-mode-map))
    (define-key map "c" 'maplev-mint-var-cont)
    (define-key map "k" 'maplev-mint-var-kill)
    map))

(defun maplev-mint-var-cont ()
  "Exit recursive-edit, to continue processing variables."
  (interactive)
  (exit-recursive-edit))

(defun maplev-mint-var-kill ()
  "Kill current line."
  (interactive)
  (let ((item (tabulated-list-delete-entry)))
    (when item
      (setq tabulated-list-entries (delq item tabulated-list-entries)))))

(define-derived-mode maplev-mint-var-mode tabulated-list-mode "vars"
  "Major mode for modifying undeclared variables."
  (setq tabulated-list-format [("*Variable*" 20 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun maplev-mint-edit-vars (vars)
  "Open a temporary buffer to edit VARS, a list of variables.
Return the edited list upon completion."
  (with-temp-buffer
    (pop-to-buffer (current-buffer))
    (maplev-mint-var-mode)
    (setq tabulated-list-entries
	  (let ((cnt 0))
	    (mapcar (lambda (var) (list (cl-incf cnt) (vector var))) vars)))
    (tabulated-list-print)
    (recursive-edit)
    ;; convert lines to list
    (goto-char (point-max))
    (unless (bobp)
      (forward-line -1)
      (let (vars)
	(while
	    (let ((var (buffer-substring-no-properties (1+ (point)) (line-end-position))))
	      (setq vars (cons var vars))
	      (unless (bobp)
		(forward-line -1))))
	vars))))

;; (defun maplev-mint-file-find ()
;;   (interactive)
;;   (let ((buffer (current-buffer))
;; 	(config maple-config)
;; 	(file
		


(provide 'maplev-mint)

;;; maplev-mint.el ends here
