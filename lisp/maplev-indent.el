;;; maplev-indent.el --- Indentation engine for MapleV

;;; Commentary:
;; 

;; The indentation functions handle the indentation of Maple code.
;; They are based on the Maple-mode package written by Nicholas
;; Thie'ry.  Considerable changes have been made to handle the
;; extended syntax introduced in Maple R6.  Following is a brief
;; description of the algorithm.
;; 
;; The buffer local list variable `maplev-indent-info' stores
;; the indentation information at a particular point, call it the
;; `known-indent-point' (the point position is stored in the list).
;; When a line is indented, the algorithm checks whether the current
;; position is greater than `known-indent-point'; if so, it only needs
;; to check between that point and the current position.  If not, it
;; needs to search backwards for a known valid indentation point.  The
;; function `maplev--validate-indent-info' handles this.
;;
;; The amount that a particular line is indented is determined by the
;; grammar defined by the constant assoc list `maplev--grammar-alist'.

;;; Code:
;;

(require 'maplev-custom)
(require 'maplev-re)
(require 'maplev-common)

(eval-when-compile
  (defvar maplev-symbol-syntax-table))


;;{{{ module

;; Define variables and functions for handling indentation information.

(defvar maplev-indent-declaration 0
  "Buffer-local variable that sets the indentation declarations.
Declarations are Maple local, global, and export statements.
The default value is taken from `maplev-indent-declaration-level'.")

(defvar maplev-indent-use-info-flag t
  "Non-nil means use `maplev-indent-info' to speed-up indentation.
May interfere with some modes (e.g. noweb).")
(make-variable-buffer-local 'maplev-indent-use-info-flag)


(defvar maplev-indent-info nil
  "Buffer local variable storing previous indent information.

Nil when there is no previous, or valid, indent information.
Otherwise it's a list: \(POINT STATE STACK\).  POINT is the
buffer character position at which the information applies.
STATE is the output of `parse-partial-sexp' \(valid from the
start of the buffer to POINT\).  STACK is a list of lists, each
list having the form \(KEYWORD INDENT-CLOSE INDENT-FOLLOW\).
KEYWORD is a keyword or parenthesis in the source.  INDENT-CLOSE
is the indentation for the closing keyword associated with
KEYWORD.  INDENT-FOLLOW is the indentation for source between
KEYWORD and its closing keyword.  Indentation is measured in
characters, with 0 being the left margin.")

;; Procedures for accessing the contents of `maplev-indent-info'.

(defsubst maplev-indent-info-point ()
  "Return position of last valid indent."
  (nth 0 maplev-indent-info))

(defsubst maplev-indent-info-state ()
  "Return output of `parse-partial-sexp' from last indent."
  (nth 1 maplev-indent-info))

(defsubst maplev-indent-info-stack ()
  "Return indentation stack."
  (nth 2 maplev-indent-info))

(defsubst maplev-indent-info-assign (point state stack)
  "Assign POINT, STATE, and STACK to the variable `maplev-indent-info'."
  (setq maplev-indent-info (list point state stack)))

(defsubst maplev-indent-clear-info ()
  "Clear the indent information."
  (interactive)
  (setq maplev-indent-info nil))

(defvar maplev-find-indent-point-function nil
  "Function to return point from which indentation can be computed.
If assigned nil or it returns nil, the usual indentation method
is used.  This function is called with no arguments.  It must
preserve point.")

(defun maplev-indent-validate-info ()
  "Update the variable `maplev-indent-info' if nil.
Set POINT in variable to closest valid starting point.
Set STATE and STACK in variable to nil."
  (unless (and
           maplev-indent-use-info-flag
           maplev-indent-info
	   (>= (point) (maplev-indent-info-point)))
    ;; Set POINT to (point) if we're at the beginning of a top level
    ;; procedure assignment, otherwise search backwards for the
    ;; beginning or end of a top level procedure assignment and put
    ;; point outside it.  If neither is found, move point to the start
    ;; of the buffer.  WHAT ABOUT NARROWING AND/OR FOLDING?
    (maplev-indent-info-assign
     (or (and (looking-at maplev--top-defun-begin-re) (point))
	 (when maplev-find-indent-point-function
	   (funcall maplev-find-indent-point-function))
         ;; (save-excursion
	 ;;   ;; Handle noweb mode.
	 ;;   ;; If noweb is active in the buffer, then search for
	 ;;   ;; the chunk starter.
	 ;;   ;;(and
	 ;;   ;;(boundp 'noweb-minor-mode) noweb-minor-mode
	 ;;   ;;(eq mmm-classes 'noweb)
         ;;   (when (re-search-backward "^<<\\(.*\\)>>=$" nil t)
         ;;     (1+ (match-end 0))))
         (save-excursion
           (when (re-search-backward
                  (concat "\\(" maplev--top-defun-begin-re "\\)\\|"
                          "\\(" maplev--top-defun-end-re-colon "\\)") nil t)
             (if (nth 2 (match-data))   ; found proc?
                 (match-beginning 0)    ;   start of proc
               (match-end 0))))         ;   end of proc
         (point-min))                   ; top of buffer
     nil nil)))

(defun maplev-indent-before-change-function (beg &rest unused)
  "Clear indent info if the buffer change is before the last info location.
This function is called whenever the buffer is changed.  BEG is the
character position of the beginning of the change.  UNUSED is not used."
  (and maplev-indent-info
       (< beg (maplev-indent-info-point))
       (maplev-indent-clear-info)))

;;}}}
;;{{{ grammar

(defconst maplev-indent-grammar-alist

  ;; Removed "in" from grammar to allow its use as a binary operator in Maple R8.
  ;; Alas, this prevents properly indenting
  ;; in iter do
  ;;    ...
  ;; end do;

  (let ((indent maplev-indent-level))
    (list
     (list "proc" . ("\\<end\\>" t indent #'maplev-indent-point-of-proc))
     (list "module" . ("\\<end\\>" t indent #'maplev-indent-point-of-proc))
     (list "end"  . (nil nil 0 nil #'maplev-indent-skip-optional-end-keyword))

     (list "for"  . ((maplev--list-to-word-re '("from" "to" "by" "while" "do")) t 0))
     (list "from" . ((maplev--list-to-word-re '("to" "by" "while" "do")) t 0))
     (list "to"   . ((maplev--list-to-word-re '("by" "while" "do")) t 0))
     (list "by"   . ((maplev--list-to-word-re '("from" "to" "while" "do")) t 0))
     (list "while" . ((maplev--list-to-word-re '("from" "to" "by" "do")) t 0))
     (list "do"   . ((maplev--list-to-word-re '("od" "end" "until")) t indent))
     (list "od"   . (nil nil 0))
     (list "until" . (nil nil 0))
     
     (list "if"   . ("\\<then\\>" t 0))
     (list "elif" . ("\\<then\\>" nil 0))
     (list "else" . ((maplev--list-to-word-re '("fi" "end")) nil indent))
     (list "then" . ((maplev--list-to-word-re '("elif" "else" "fi" "end")) nil indent))
     (list "fi"   . (nil nil 0))
     
     (list "use"  . ("\\<end\\>" t indent))
     (list "try"  . ((maplev--list-to-word-re '("catch" "finally" "end")) t indent))
     (list "catch".  ((maplev--list-to-word-re '("catch" "finally" "end")) t indent))
     (list "finally".  ((maplev--list-to-word-re '("end")) t indent))
     
     (list "{"    . ("}" t nil))
     (list "["    . ("]" t nil))
     (list "("    . (")" t nil))
     (list "}"    . (nil nil 0))
     (list "]"    . (nil nil 0))
     (list ")"    . (nil nil 0))))

  "Assoc list defining the grammar for Maple indentation.
Each entry has the form \(KEY . \(MATCH-RE OPEN-P INDENT
ADJUST-FUNC POST-FUNC\)\).

KEY is a Maple keyword or parenthesis.

MATCH-RE is a regular expression that matches any of the keys
that follow KEY; nil means that KEY closes a Maple statement.

OPEN-P is a boolean flag that is non-nil if KEY can initiate a
Maple statement.

INDENT is the relative indentation for the block immediately
following KEY; nil means that the indentation is handled in an ad
hoc fashion.

ADJUST-FUNC is optional; if non-nil it is a function that moves
point to the position from where the indent is computed.

POST-FUNC is optional; if non-nil it is a function that is called
after the keyword is handled.  Currently it is only used by the
keyword `end'.")

(defconst maplev-indent-grammar-keyword-re
  (eval-when-compile
    (concat
     (maplev--list-to-word-re
      '("proc" "module" "end"
        "for" "from" "to" "by" "while" "do" "break" "next" "od" "until"
        "if" "elif" "else" "then" "fi"
        "use" "try" "catch" "finally"))
     "\\|\\("
     (regexp-opt '("{" "}" "[" "]" "(" ")" "(*" "*)" ))
     "\\)"))
  "Regular expression of keywords used in Maple grammar for indentation.")

(defun maplev-indent-skip-optional-end-keyword ()
  "Skip the optional keyword following an end statement."
  (when (looking-at (concat "[ \t]+"
			    (maplev--list-to-word-re '("proc" "module" "do" "use" "if" "try"))))
    (goto-char (match-end 0))))

;;}}}
;;{{{ errors

;; Create a new error symbol, `keyword-out-of-sequence', for handling
;; keywords and parentheses that appear out of sequence during an
;; indentation.  It isn't clear to me that this is the proper way to
;; handle this rather special condition; but I'll go with it for now.

(put 'keyword-out-of-sequence
     'error-conditions
     '(error keyword-out-of-sequence))

(put 'keyword-out-of-sequence 'error-message "Keyword out of sequence")

(defun maplev-indent-handle-grammar-error (err)
  "Handle a grammar error ERR.
Push the mark \(so that we can return to it with \\[universal-argument] \\[set-mark-command]\),
ding the bell, display a message, and move point to the
start of the offending keyword."
  (push-mark)
  (ding)
  (message "Keyword `%s' out of sequence" (nth 1 err))
  (goto-char (nth 2 err)))

;;}}}
;;{{{ functions

(defun maplev-goto-previous-codeline ()
  "Move point to the start of the previous line of Maple code.
Blank lines and comment lines are skipped.
THIS WILL FAIL IN A STRING."
  (interactive)
  (while (and (= (forward-line -1) 0)
              (looking-at "\\s-*\\(#\\|$\\)"))))

(defun maplev-indent-point-of-proc ()
  "Move point to position from where a procedure is indented.
Point must originally be just to the left of the \"proc\" or \"module\".
If procedure is anonymous, point is not moved and nil is returned.
Otherwise point is moved to left of assignee and point is returned."
  ;; Regexp does not include possible comments.
  (and (re-search-backward (concat maplev--possibly-typed-assignment-re "\\=") nil t)
       (goto-char (match-beginning 1))))

(defun maplev-indent-line-with-info ()
  "Indent the current line as Maple code.  Point must be at the left margin."
  (unless (or (and maplev-dont-indent-re
                   (looking-at maplev-dont-indent-re))
              (let ((state (maplev-indent-info-state)))
                (or (nth 3 state) (nth 4 state))))
    (delete-region (point) (progn (skip-chars-forward " \t") (point)))
    (indent-to (maplev-indent-compute (car (maplev-indent-info-stack))))))

;;}}}
;;{{{ algorithm

;; Algorithm:

;; The indentation algorithm is intended to provide rapid indentation
;; both for interactive use, that is, using `maplev-indent-newline',
;; and for global use, that is, using `maplev-indent-region'.
;;
;; To rapidly indent a region, previous indentation information is
;; stored in data structure, `maplev-indent-info'.  See its docstring
;; for a description of the structure.  To interactively indent, the
;; data is checked to see if there is usable information.  If so, it
;; is used, otherwise the nearest preceding syntactically
;; grammatically point (the start or end of a top level procedure
;; assignment) is found and the indentation information computed from
;; that point.


(defun maplev-indent-update-info ()
  "Update the variable `maplev-indent-info' at point.
Scan the source for keywords and parentheses from the previous valid
indent position to point.  Update the stack and state according to the
syntax table and the grammar, `maplev-indent-grammar-alist'.  Restore point.
The calling function must ensure that the previous info point is not
beyond \(point\)."

  ;; This uses unwind-protect to restore the syntax table.
  ;; Why not use with-syntax-table instead?  One excuse for
  ;; not changing this is that with-syntax-table is more complicated,
  ;; it uses unwind-protect as well as save-current-buffer.
  (save-excursion
    (let ((point (maplev-indent-info-point))
          (stack (maplev-indent-info-stack))
          (state (maplev-indent-info-state))
          (end (point))

          keyword keyword-beginning key-list indent indent-close
          adjust-func post-func top-stack old-keyword match-re
          case-fold-search)

      (unwind-protect
          (save-restriction
            (widen)

	    ;; Change the buffer syntax table to maplev-symbol-syntax-table
	    ;; so that the underscore is considered a word constituent.
	    (with-syntax-table maplev-symbol-syntax-table
	      
	      (goto-char point)
	      (while (re-search-forward maplev-indent-grammar-keyword-re end 'move)
		
		;; Assign loop variables.  KEY-POINT is assigned the position
		;; after the next keyword.  If no keyword exists in the line,
		;; KEY-POINT is nil.
		
		(setq keyword (match-string-no-properties 0)
		      key-list (cdr (assoc keyword maplev-indent-grammar-alist))
		      indent (nth 2 key-list)
		      adjust-func (nth 3 key-list)
		      post-func (nth 4 key-list)
		      top-stack (car stack)
		      indent-close (nth 1 top-stack)
		      old-keyword (car top-stack) ; Don't set to (old) KEYWORD, it might have been matched
		      match-re (and old-keyword
				    (car (cdr (assoc old-keyword maplev-indent-grammar-alist))))
		      keyword-beginning (match-beginning 0)
		      state (parse-partial-sexp point (point) nil nil state)
		      point (point))
		(cond

		 ;; If KEYWORD is in a comment or a quote, do nothing.
		 ((or (nth 4 state) (nth 3 state) (string= keyword "*)"))) ; comments are more frequent, so check first
		 
		 ;; Does KEYWORD pair with the top one on STACK?
		 ((and match-re (string-match match-re keyword))
		  ;; Should more keywords follow KEYWORD?
		  (if (nth 0 key-list)
		      ;; If so, replace the top of STACK with a new list.  The
		      ;; new list has the new KEYWORD, the INDENT-CLOSE from
		      ;; the old list, and
		      (setcar stack (list keyword
					  indent-close
					  (+ indent-close indent)))
		    ;; otherwise pop the top of STACK.
		    (and post-func (funcall post-func))
		    (setq stack (cdr stack))))

		 ;; Is KEYWORD an opening keyword?  Push a new item onto
		 ;; STACK.

		 ((nth 1 key-list)
		  ;; If keyword is an opening parenthesis, find the matching closing parenthesis,
		  ;; if it occurs before 'end', jump to it and skip all intermediate matches.
		  (unless (and (memq (aref keyword 0) '(?\[ ?{ ?\())    ; check first character
			       (let ((pt (condition-case nil ; get position of closing parenthesis
					     (scan-lists (1- point) 1 0)
					   (error nil))))
				 (and pt (< pt end)
				      (goto-char pt))))
		    (setq stack
			  (cons
			   (cons
			    keyword
			    ;; Handle keywords and parentheses appropriately.
			    ;; Indentation for keywords that
			    ;; start a Maple statement is from
			    ;; `keyword-beginning'; however, if the
			    ;; keyword is an assigned proc then the actual
			    ;; beginning of the keyword is the start of
			    ;; the assigned name.
			    (if indent
				(save-excursion
				  (goto-char keyword-beginning)
				  (and adjust-func (funcall adjust-func))
				  (list (current-column) ; alignment for closing keyword
					(+ (current-column) indent))) ; alignment for subblock

			      ;; Handle an open parenthesis.  INDENT-CLOSE is
			      ;; set to the same column as the parerenthesis so
			      ;; that the closing parenthesis is aligned.  If
			      ;; space or a a comment follows the parenthesis,
			      ;; then the following block of code is indented
			      ;; from the current indentation.  Otherwise
			      ;; following code indents to first character
			      ;; following the parenthesis.
			      (list
			       (1- (current-column)) ; INDENT-CLOSE
			       (progn
				 (skip-chars-forward " \t")
				 (if (looking-at "#\\|$") ; no code on remainder of line
				     (+ (current-indentation) maplev-indent-level)
				   (current-column))))))
			   stack))))

		 ;; Check whether keyword is break or next.  Skip following if.
		 ((or (string= keyword "break") (string= keyword "next"))
		  (when (looking-at "\\s-+if\\>")
		    (goto-char (match-end 0))))

		 ;; KEYWORD is out of sequence.  Move point before KEYWORD and
		 ;; signal an error.
		 (t (re-search-backward keyword)
		    (signal 'keyword-out-of-sequence (list keyword (point))))))
	      (if (< point end)
		  (setq state (parse-partial-sexp point (point) nil nil state)))
	      (maplev-indent-info-assign end state stack)))))))

;;}}}
;;{{{ commands

(defun maplev-indent-compute (indent-info)
  "Return the indentation required for a Maple code line.
INDENT-INFO is the indentation information applicable to this line;
it is a list of three items: \(KEYWORD INDENT-CLOSE INDENT-FOLLOW\).
See `maplev-indent-info' for details.  If INDENT-INFO is nil then 0
is returned.  Point must be at current indentation."
  (if (not indent-info)
      0
    (save-excursion
      (let ((point (point))
            case-fold-search)
        (cond
         ;; Handle declarations in procedures (and modules)
         ((and (string-match maplev--defun-re (car indent-info))
               (with-syntax-table maplev-symbol-syntax-table
                 (looking-at maplev--declaration-re)))
          (+  maplev-indent-declaration
              (nth 1 indent-info)))
         ;; Continued dotted quotes, e.g. ``."a string".''
         ;; They are aligned with previous quoted material.
         ;; There should be a flag to disable this.
         ((and
           (looking-at maplev--space-dot-quote-re)
           (not (bobp))
           (save-excursion
             (maplev-goto-previous-codeline)
             (setq point (point))
             (end-of-line)
             (setq point (re-search-backward maplev--quote-re point 'move))))
          (goto-char point)
          (max 0 (1- (current-column))))

         ;; We've handled the special cases.
         ;; Now to tackle regular statements.
         (t
          (or
           (let* ((old-keyword (car indent-info))
                  (match (and old-keyword (nth 1 (assoc old-keyword maplev-indent-grammar-alist)))))
             (nth (if (and match (looking-at match))
                      1
                    2)
                  indent-info))
           0)))))))                     ; maplev-indent-compute

(defun maplev-indent-region (beg end)
  "Indent the region between POINT and MARK.
BEG and END may also be passed to the function."
  (interactive "r")
  (condition-case err
      (save-excursion
        (let ((before-change-functions nil)
              (after-change-functions nil))
          ;; Clear the indent stack.  Goto to the start of the region.
          ;; Set up a marker for the end of the region (it is used to
          ;; compute the percent completed).
          (goto-char beg)
          (beginning-of-line)
          (setq end (set-marker (make-marker) end))
          (maplev-indent-clear-info)   ; temporary
          (maplev-indent-validate-info)

          ;; THE FOLLOWING LINE IS EXPERIMENTAL BUT SEEMS NECESSARY
          (maplev-indent-update-info)
          ;; Indent each line in the region

          (while (and (<= (point) end) (not (eobp)))
            (maplev-indent-line-with-info)
            (forward-line)
            (maplev-indent-update-info)
            (message "Indenting...(%d%%)"
                     (min 100 (* 10 (/ (* 10 (- (point) beg)) (- end beg))))))

          (message "Indenting...done")
          (set-marker end nil)))

    (keyword-out-of-sequence
     (maplev-indent-handle-grammar-error err)))) ; {end} maplev-indent-region


(defun maplev-indent-buffer ()
  "Indent the buffer."
  (interactive)
  (save-restriction
    (widen)
    (maplev-indent-region (point-min) (point-max))))

(defun maplev-indent-procedure ()
  "Indent the current procedure or module."
  (interactive)
  (apply 'maplev-indent-region (maplev-current-defun)))

(defun maplev-indent-line ()
  "Indent current line according to grammar.
If point was to the left of the initial indentation, it moves to the
final indentation; otherwise it remains in the same position relative
to the indentation."
  (interactive)
  ;; 25-Feb-2001: Added condition-case to move cursor to an out of sequence keyword.
  (condition-case err
      (let ((before-change-functions nil))
        (goto-char (max (save-excursion
                          (beginning-of-line)
                          (maplev-indent-validate-info)
                          (maplev-indent-update-info)
                          (maplev-indent-line-with-info)
                          (point))
                        (point))))
    (keyword-out-of-sequence
     (maplev-indent-handle-grammar-error err))))

;; This is used by `indent-for-comment' to decide how much to indent a
;; comment in Maple code based on its context.

(defun maplev-indent-comment-indentation ()
  "Return the column at which a comment should be started or moved to.
If the line starts with a flush left comment, return 0."
  (if (looking-at "^#")
      0                         ; Existing comment at bol stays there.
    comment-column))

;;}}}

(provide 'maplev-indent)

;;; maplev-indent.el ends here
