;;; maplev-mint.el --- Syntax checking for Maple

;;; Commentary:
;; 

;;; Code:
;;

(require 'eieio)
(require 'maplev-config)
(require 'maplev-re)

(eval-when-compile
  (defvar maplev-add-declaration-function)
  (defvar maplev-alphabetize-declarations-p)
  (defvar maplev-var-declaration-symbol)
  (defvar maplev-variable-spacing))

(declare-function event-point "maplev-common")
(declare-function event-window "maplev-common")
(declare-function maplev--end-of-defun-pos "maplev-common")
(declare-function maplev--ident-around-point "maplev-common")
(declare-function maplev-beginning-of-defun "maplev-common")
(declare-function maplev-current-defun "maplev-common")
(declare-function maplev-find-include-file "maplev")
(declare-function maplev-ident-around-point-interactive "maplev-common")
(declare-function maplev-indent-line "maplev-indent")
(declare-function maplev-indent-newline "maplev")
(declare-function maplev-expand->file-name "maplev")

;;{{{ customizable variables

(defcustom maplev-mint-coding-system 'undecided-dos
  "Coding system used by Mint.  See `coding-system-for-read' for details."
  ;; TBD: why is this customizable?
  :type '(choice (const undecided-dos) (const raw-text-unix) (symbol :tag "other"))
  :group 'maplev-mint)

(defcustom maplev-mint-query t
  "Non-nil means query before correcting."
  :type 'boolean
  :group 'maplev-mint)

(defcustom maplev-mint-process-all-vars nil
  "Non-nil means process all variables in one step."
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

(defvar maplev-mint-mode-syntax-table nil
  "Syntax table used in Maple mint buffer.")
(unless maplev-mint-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?[  "w"  table)
                         (modify-syntax-entry ?]  "w"  table)
    (modify-syntax-entry ?_  "w"  table)
    (modify-syntax-entry ?/  "w"  table)
    (modify-syntax-entry ?\` "\"" table) ; string quotes
    (setq maplev-mint-mode-syntax-table table)))

;;}}}
;;{{{ mode map

(defvar maplev-mint-mode-map nil
  "Keymap used in Mint mode.")

(unless maplev-mint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(space)]                     'scroll-up)
    (define-key map [(backspace)]                 'scroll-down)
    (define-key map [(return)]                    'maplev-mint-rerun)
    (define-key map [(control c) (return) return] 'maplev-mint-rerun)
    (define-key map [?q]                          'quit-window)
    (define-key map [?s]                          'isearch-forward)
    (define-key map [?r]                          'isearch-backward)
    (define-key map [(mouse-1)]                   'maplev-mint-click)
    (define-key map [(control c) (control c)]     'maplev-mint-handler)
    (setq maplev-mint-mode-map map)))

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
  (maplev-mint-fontify-buffer))

(defun maplev-mint-setup (code-buffer config)
  "Unless already assigned, set `major-mode' to `maplev-mint-mode'.
Set `maplev-mint--code-buffer' to CODE-BUFFER, the buffer that
contains the source code.  Set `maplev-config' to CONFIG."
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
and move forward L lines and C columns."
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
  (when (re-search-backward "included: ")
    (goto-char (match-end 0))
    (when (looking-at ".*$")
      (let ((file (maplev-find-include-file 
		   (match-string-no-properties 0) 
		   'inc-first (slot-value maplev-config 'include-path))))
	(when file
	  (find-file-other-window file))))))
  

(defun maplev-mint--goto-source-proc (pos)
  "Move to position in source buffer corresponding to link at POS in mint buffer.
This position is after the formal parameter list of the operator,
procedure, or module."

  ;; find the line number of the source buffer at which the defun starts
  (goto-char pos)
  (let (line file class)
    (save-excursion
      (re-search-backward "^\\(Nested \\)?\\(Anonymous \\)?\\(Procedure\\|Operator\\|Module\\)")
      ;; Assign class Procedure, Operator, or Module
      (setq class (match-string-no-properties 3))
      (re-search-forward "on\\s-*lines?\\s-*\\([0-9]+\\)")
      (setq line (1- (string-to-number (match-string-no-properties 1)))
	    file (maplev-mint-get-source-file))
      ;; move point to the beginning of that line in the source
      (maplev-mint--goto-source-pos line 0 file))
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
	(goto-char (match-beginning 0)))))))

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
      (maplev-find-include-file file 'inc-first (slot-value maplev-config 'include-path)))))

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
  '(("\\(^on line[ \t]*[0-9]+:\\)" maplev-mint-note-face)
    ("^[ \t]*\\(\\^.*$\\)" maplev-mint-error-face 'error)
    ("^\\(?:Nested \\)?\\(?:Procedure\\|Operator\\|Module\\)[ ]*\\([^(]*\\)" maplev-mint-link-face 'proc)
    ("^\\(?:Nested \\)?Anonymous \\(?:Procedure\\|Operator\\|Module\\)[ ]*\\(\\(?:proc\\|module\\) *([^)]*)\\)" maplev-mint-link-face 'proc)
    ("^This source file is included: \\(.*\\)" maplev-mint-link-face 'include-file)
    ("These parameters were never used\\(?: explicitly\\)?:" maplev-mint-warning-face 'unused-arg t)
    ("These names appeared more than once in the parameter list:" maplev-mint-warning-face 'repeat-arg t)
    ("These local variables were not declared explicitly:" maplev-mint-warning-face 'undecl-local t)
    ("These local variables were never used:" maplev-mint-warning-face 'unused-local t)
    ;;("These local variables were assigned a value, but otherwise unused:" ... )
    ("These names were declared more than once as a local variable:" maplev-mint-warning-face 'repeat-local t)
    ("These names were used as global names but were not declared:" maplev-mint-warning-face 'undecl-global t)
    ("\\(on line +[0-9]+\\)" maplev-mint-link-face 'goto-line)
   ; ("\\(on lines +[0-9]+\\s-+to\\s-++[0-9]+\\s-+of\\s-+.*\\)" maplev-mint-note-face 'goto-line)
    ;; Could we make the following optional?
    ;; ("Global names used in this procedure:"
    ;;  1 maplev-mint-warning-face 'undecl-global t)
    )
  "Alist for fontification in a Mint buffer.
Each element is a list of the form \(REGEXP FACE PROP VAR\),
where REGEXP is to be matched and FACE is a face.  Optional third
element PROP is a symbol used for marking the category of SUBEXP.
Optional fourth element VAR is non-nil if REGEXP is catenated
with `maplev-mint-variables-re'.  The text that matches the first
group of the full regular expression is given face property face
and text property PROP.")

(defun maplev-mint-fontify-buffer ()
  "Fontify the mint buffer.  Does not use font-lock mode."
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
          (put-text-property beg end 'face (eval (nth 1 mel)))
          (when (nth 2 mel)
            ;; We use a text property `maplev-mint' to store in the text
            ;; what kind of info we have from Mint.
            (put-text-property beg end 'maplev-mint (eval (nth 2 mel)))
            (if (and (nth 3 mel)
                     (not maplev-mint-process-all-vars)) ; then we do highlighting word-wise
                (save-excursion
                  (goto-char beg)
                  ;; Slightly simpler algorithm than the one used by
                  ;; maplev--ident-around-point to pick up the word
                  ;; where point is.  Does it matter for highlighting?
                  ;;                   (while (re-search-forward "\\<\\w+\\>" end t)
                  ;;                     (put-text-property (match-beginning 0) (match-end 0)
                  ;;                                        'mouse-face 'highlight)))
                  (while (re-search-forward "\\<\\(\\w+\\)\\>" end t)
                    (put-text-property (match-beginning 1) (match-end 1)
                                       'mouse-face 'highlight)))
              (put-text-property beg end 'mouse-face 'highlight)))))
      (setq mlist (cdr mlist)))
    (set-buffer-modified-p nil)))

;;}}}
;;{{{ interactive functions

(defun maplev-mint-query (form &rest vars)
  "Return t if correction suggested by mint should be made.
FORM and VARS are used for `y-or-n-p' query."
  (or (not maplev-mint-query)
      (y-or-n-p (apply 'format form vars))))

(defun maplev-mint-click (click)
  "Move point to CLICK."
  (interactive "e")
  (set-buffer (window-buffer (select-window (event-window click))))
  (maplev-mint-handler (event-point click)))

(defun maplev-mint-handler (pos)
  "Handle mint output at position POS.
When called interactively, POS is position where point is."
  (interactive "d")
  (let ((prop (get-text-property pos 'maplev-mint)))
    (if prop
        (let (string vars)
          (if maplev-mint-process-all-vars
              (let ((str (buffer-substring-no-properties
                          (next-single-property-change pos 'maplev-mint)
                          (previous-single-property-change (1+ pos) 'maplev-mint))))
                ;; string is like str, but with maplev-variable-spacing
                ;; vars is a comma separated list of names extracted from str
                (while (and (not (string= str ""))
                            (string-match "\\<\\w+\\>" str))
                  (setq vars (cons (match-string 0 str) vars)
                        string (if string
                                   (concat string ","
                                           (make-string maplev-variable-spacing ?\ )
                                           (match-string 0 str))
                                 (match-string 0 str))
                        str (substring str (match-end 0)))))
            (setq string (save-excursion
                           (goto-char pos)
                           (maplev--ident-around-point))
                  vars (list string)))
          ;;
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
            (when (maplev-mint-query "Delete `%s' from argument list? " string)
              (maplev-mint--goto-source-proc pos)
              (maplev-delete-vars (maplev--scan-lists -1) (point) vars)))
           ;;
           ;; Remove unused local variables from local declaration.
           ((eq prop 'unused-local)
            (when (maplev-mint-query "Delete `%s' from local statement? " string)
              (maplev-mint--goto-source-proc pos)
              (maplev-delete-declaration "local" vars)))
           ;;
           ;; Remove repeated args from argument list.
           ((eq prop 'repeat-arg)
            (when (maplev-mint-query "Remove duplicate `%s' from parameters? " string)
              (maplev-mint--goto-source-proc pos)
              (maplev-delete-vars (maplev--scan-lists -1) (point) vars 1)))
           ;;
           ;; Remove repeated local variables from local declaration.
           ((eq prop 'repeat-local)
            (when (maplev-mint-query "Remove duplicate `%s' from local statement? " string)
              (maplev-mint--goto-source-proc pos)
              (maplev-delete-declaration "local" vars 1)))
           ;;
           ;; Declaration of undeclared locals variables.
           ((eq prop 'undecl-local)
            (when (maplev-mint-query "Add `%s' to local statement? " string)
              (maplev-mint--goto-source-proc pos)
              (maplev-add-declaration "local" string)))
           ;;
           ;; Declaration of undeclared global variables.
           ((eq prop 'undecl-global)
            (when (maplev-mint-query "Add `%s' to global statement? " string)
              (maplev-mint--goto-source-proc pos)
              (maplev-add-declaration "global" string)))
           ;;
           ;; Goto line
           ((eq prop 'goto-line)
            (maplev-mint--goto-source-line pos))
           )))))

;;}}}
;;{{{ regions

(defun maplev-mint-region (beg end &optional syntax-only)
  "Run Mint on the current region \(from BEG to END\).
Return exit code of mint."
  (interactive "r")
  (let ((code-buffer (current-buffer))
        (code-window (get-buffer-window (current-buffer)))
        (coding-system-for-read maplev-mint-coding-system)
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
      (setq status (funcall #'call-process-region
			    (point-min) (point-max)
			    (slot-value config 'mint) 
			    nil ; do not delete
			    mint-buffer
			    nil  ; do not redisplay
			    (mapconcat 'identity
				       (list "-q"
					     (if syntax-only "-S")
					     (maplev-get-option-with-include config :mint-options))
				       " ")))
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
      (switch-to-buffer-other-window mint-buffer))
    status))

(defun maplev-mint-buffer ()
  "Run Mint on the current buffer."
  (interactive)
  (save-restriction
    (widen)
    (maplev-mint-region (point-min) (point-max))))

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
This function is like `re-search-forward', but comments are ignored.
Optional arguments BOUND, NOERROR, and COUNT have the same meaning
as in `re-search-forward'."
  ;; This approach gets confused by a comment inside the match
  ;; (e.g., when REGEXP can match more than one line).
  ;; Therefore it's better to break complex REGEXP's apart
  ;; and handle the items seperately.
  (if (not count) (setq count 1))
  (let ((dir (if (< count 0) -1 1))
        (pos (point))
        case-fold-search)
    (while (and (not (zerop count)) pos)
      (setq pos (re-search-forward regexp bound noerror dir))
      (while (and (nth 4 (parse-partial-sexp (maplev-safe-position) (point)))
                  (setq pos (re-search-forward regexp bound noerror dir))))
      (setq count (- count dir)))
    pos))
      
(defun maplev--re-search-backward (regexp &optional bound noerror count)
  "Search backward from point for regular expression REGEXP.
This function is like `re-search-backward', but comments are ignored.
Optional arguments BOUND, NOERROR, and COUNT have the same meaning
as in `re-search-backward'."
  ;; See maplev--re-search-forward.
  (if (not count) (setq count 1))
  (let ((dir (if (< count 0) -1 1))
        (pos (point))
        case-fold-search)
    (while (and (not (zerop count)) pos)
      (setq pos (re-search-backward regexp bound noerror dir))
      (while (and (nth 4 (parse-partial-sexp (maplev-safe-position) (point)))
                  (setq pos (re-search-backward regexp bound noerror dir))))
      (setq count (- count dir)))
    pos))

(defun maplev-safe-position (&optional to)
  "Search for safe buffer position before point \(a position not in a comment\).
Optional arg TO initializes the search.  It defaults to point"
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
  "Goto the just after the next statement terminator."
  (save-excursion
    (maplev--re-search-forward "[^:]\\(;\\|:[^-:=]\\)" nil t)
    (+ 1 (match-beginning 1))))

(defun maplev--goto-declaration (keyword)
  "Move point to the start of the KEYWORD declaration in a Maple procedure.
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
        (goto-char (match-beginning 0)))))


(defun maplev-add-declaration-one-line (keyword var)
  "To the current procedure's KEYWORD declaration add VAR.
If necessary, add a KEYWORD statement.  Point must be after the closing
parenthesis of the procedure's argument list."
  (save-excursion
    (if (maplev--goto-declaration keyword)
        (let ((end (maplev--statement-terminator)))
          (if maplev-alphabetize-declarations-p
              (progn
                (forward-word)
                (while (and (< (point) end)
                            (looking-at (concat "\\s-*\\(" maplev--symbol-re "\\) *,?"))
                            (string< (match-string-no-properties 1) var))
                  ;; Move over symbol and, if there, comma.  For now
                  ;; assume that the symbols do not have
                  ;; types---including types in-line seems bad form.
                  ;; Moving across a type is fairly tricky.
                  (goto-char (match-end 0)))
                (if (looking-at " *;")
                    (insert (format ", %s" var))
                  (insert (format " %s," var))))
            (goto-char end)
            (backward-char)
            (insert "," (make-string maplev-variable-spacing ?\ ) var)))
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
      ;; Insert the declaration statement KEYWORD VAR ; at point.
      ;; If point is at beginning of line, insert a newline at end.
      ;; NOTE: It might be better to look whether there is any following text.
      (let ((new-line (bolp)))
        (insert keyword " " var "; ")
        (when new-line
          (newline)
          (forward-line -1)))
      (maplev-indent-line))))

(defun maplev-add-declaration-leading-comma (keyword var)
  "To the current procedure's KEYWORD declaration add VAR.
If necessary, add a KEYWORD statement.  Point must be after the
closing parenthesis of the procedure's argument list.  Optional
TYPE specifies the declared type of VAR.  The string
`maplev-var-declaration-symbol' is used as the declaration
symbol; it is inserted between VAR and TYPE.

If `maplev-alphabetize-declarations-p' is non-nil, VAR is
inserted alphabetically into the sequence of existing
declarations, otherwise it is inserted at the end."
  (save-excursion
    (if (maplev--goto-declaration keyword)
        (let ((end (maplev--statement-terminator)))
          (if maplev-alphabetize-declarations-p
              (progn
                (forward-word)
                (while (and (< (point) end)
                            (looking-at (concat "\\s-*\\(?:, \\)?\\(" maplev--symbol-re "\\)"))
                            (string< (match-string-no-properties 1) var))
                  (forward-line))
                (if (looking-at "\\s-*[,;]")
                    (progn
                      (insert (format ", %s" var))
                      (maplev-indent-newline))
                  (insert (format " %s" var))
                  (maplev-indent-newline)
                  (insert ", ")))
            ;; Insert `, VAR' before terminator.
            (goto-char end)
            (forward-line 0)
            (insert (format ", %s" var))
            (maplev-indent-newline)))
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
          (forward-line)          ; Else move to the next code line.
          (unless stay            ; Keep moving if we not already
            (while (looking-at "[ \t]*#") ; have a declaration.
              (forward-line)))))
      ;; Insert the declaration statement KEYWORD VAR\n; at point.
      (insert (format " %s %s" keyword var))
      (maplev-indent-newline)
      (insert ";")
      (maplev-indent-newline))))

(defun maplev-add-declaration-trailing-comma (keyword var)
  "To the current procedure's KEYWORD declaration add VAR.
If necessary, add a KEYWORD statement.  Point must be after the
closing parenthesis of the procedure's argument list.  Optional
TYPE specifies the declared type of VAR.  The string
`maplev-var-declaration-symbol' is used as the declaration
symbol; it is inserted between VAR and TYPE.

If `maplev-alphabetize-declarations-p' is non-nil, VAR is
inserted alphabetically into the sequence of existing
declarations, otherwise it is inserted at the end."
  (save-excursion
    (if (maplev--goto-declaration keyword)
        (let ((end (maplev--statement-terminator)))
          (if maplev-alphabetize-declarations-p
              (progn
                (forward-line)
                (while (and (< (point) end)
                            (looking-at (concat "\\s-*\\(" maplev--symbol-re "\\)"))
                            (string< (match-string-no-properties 1) var))
                  (forward-line))
                (if (< (point) end)
                    (insert (format "%s," var))
                  (forward-line -1)
                  (search-forward ";" (line-end-position))
                  (replace-match ",")
                  (forward-line)
                  (insert (format "%s;" var)))
                (maplev-indent-newline))
            ;; Replace terminator with comma.
            ;; Preserve comments following terminator.
            (goto-char end)
            (delete-char -1)
            (insert ",")
            (end-of-line)
            ;; Insert VAR with terminator.
            (newline)
            (insert (format "%s;" var))
            (maplev-indent-line)))
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
          (forward-line)          ; Else move to the next code line.
          (unless stay            ; Keep moving if we not already
            (while (looking-at "[ \t]*#") ; have a declaration.
              (forward-line)))))
      ;; Insert the declaration statement KEYWORD VAR ; at point.
      ;; If point is at beginning of line, insert a newline at end.
      ;; NOTE: It might be better to look whether there is any following text.
      (if (bolp)
          (progn
            (insert keyword)
            (maplev-indent-line)
            (newline)
            (insert (format "%s;" var))
            (newline)
            (forward-line -1))
        (insert (format " %s %s;" keyword var) ))
      (maplev-indent-line))))

(defun maplev-add-declaration (keyword var &optional type)
  "To the KEYWORD declaration of current procedure, add VAR.
If non-nil, optional TYPE is catenated to VAR and
`maplev-var-declaration-symbol'.  The function assigned to
`maplev-add-declaration-function' does the work."
  (funcall maplev-add-declaration-function keyword
           (if type
               (concat var maplev-var-declaration-symbol type)
             var)))

(defun maplev-add-local-variable (var &optional type)
  "Add VAR to the current procedure's local statement.
If TYPE is non-nil, add a type declaration.  Interactively, VAR
defaults to the identifier point is on."
  (interactive (list (maplev-ident-around-point-interactive
                      "Local variable")
                     ;; Query type unless all declarations go on one line.
                     ;; It seems bad from to use types one one-line; regardless,
                     ;; `maplev-add-declaration-one-line' currently does not
                     ;; handle types.
                     (unless (eq maplev-add-declaration-function 'maplev-add-declaration-one-line)
                       (let ((type (read-string
                                    "Type (empty for no type): "
                                    nil
                                    maplev--declaration-history)))
                         (if (equal "" type)
                             nil
                           type)))))
  (maplev-add-variable "local" var type))
                                 

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

(defun maplev-add-variable (keyword var &optional type)
  "To the current procedure's KEYWORD declaration add VAR.
If TYPE is non-nil, append a type declaration."
  (save-excursion
    (maplev-beginning-of-defun)
    (goto-char (maplev--scan-lists 1))
    (maplev-add-declaration keyword var type)))

(defun maplev-delete-declaration (keyword vars &optional leave-one)
  "From the KEYWORD declaration delete occurrences of VARS.
VARS must be either a string or a list of strings.  If optional
argument LEAVE-ONE is non-nil, then one occurrence of VARS is left.
The entire statement is deleted if it is left with no variables."
  (save-excursion
    (when (maplev--goto-declaration keyword)
      (maplev-delete-vars (point) (maplev--statement-terminator)
                          vars leave-one)
      ;; remove entire KEYWORD statement, if empty
      (let (case-fold-search)
        (when (looking-at (concat keyword "[ \t\n]*[;:]\\([ \t#]*$\\)?"))
          (delete-region (match-beginning 0) (match-end 0))
          (maplev-delete-whitespace t))))))

(defun maplev-delete-vars-old (start end vars &optional leave-one)
  "In region between START and END delete occurrences of VARS.
VARS must be either a string or a list of strings.  If optional
argument LEAVE-ONE is non-nil, then one occurrence of VARS is left."
  (let (case-fold-search lo)
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (if (stringp vars) (setq vars (list vars)))
        (while vars
          (setq lo leave-one)
          (goto-char (point-min))
          (while (maplev--re-search-forward
                  (concat "\\<" (car vars) "\\>"
                          ;; Add optional type declarations.  I don't know
                          ;; how to make this robust, a type
                          ;; declaration can have commas and closing
                          ;; parentheses.
                          "\\(\\s-*::\\s-*[^,:;)]+\\)?")
                  nil t)
            (if lo
                (setq lo nil)
              (delete-region (match-beginning 0) (match-end 0))
              (maplev-delete-whitespace)
              (when (or (maplev--re-search-forward  "," nil t)
                        (maplev--re-search-backward "," nil t))
                (delete-region (match-beginning 0) (match-end 0))
                (maplev-delete-whitespace))))
          (setq vars (cdr vars)))))))

(defun maplev-delete-vars (start end vars &optional leave-one)
  "In region between START and END delete occurrences of VARS.
VARS must be either a string or a list of strings.  If optional
argument LEAVE-ONE is non-nil, then one occurrence of VARS is left."
  (let ((parse-sexp-ignore-comments)
        case-fold-search lo )
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (if (stringp vars) (setq vars (list vars)))
        (while vars
          (setq lo leave-one)
          (goto-char (point-min))
          (while (maplev--re-search-forward
                  (concat "\\<" (car vars) "\\>")
                  nil t)
            (if lo
                (setq lo nil)
              (delete-region (match-beginning 0) (match-end 0))
              (maplev-delete-whitespace)

              ;; Remove optional type declaration
              
              (when (looking-at "::\\s-*")
                ;; Skip past type declaration operator (::)
                ;; so looking-at won't match them.
                (goto-char (match-end 0))
                (delete-region (match-beginning 0)
                               (progn
                                 ;; Unless looking at an argument separator,
                                 ;; statement terminator, or closing
                                 ;; parenthesis, or at end of buffer, move
                                 ;; forward over a balanced expression.
                                 ;;
                                 ;; This nees modification to handle comments,
                                 ;; esp. with leading commas.
                                 (while (and (not (looking-at "[ \t\f\n]*[,;:#)]"))
                                             (/= (point) (point-max)))
                                   (forward-sexp))
                                 (point))))
              ;; Remove separating comma
              (when (or (maplev--re-search-backward "," nil t)
                        (maplev--re-search-forward  "," nil t))
                (delete-region (match-beginning 0) (match-end 0))
                (maplev-delete-whitespace))))
          (setq vars (cdr vars)))))))

;;}}}

(provide 'maplev-mint)

;;; maplev-mint.el ends here
