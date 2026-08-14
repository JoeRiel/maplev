;;; maplev-common.el --- Common functions for maplev -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
;;

(require 'maplev-compat)
(require 'maplev-custom)
(require 'maplev-re)

;;{{{ Suppress byte-compile warnings

(eval-when-compile
  (defvar maplev-mode-syntax-table)
  (defvar maplev-mode-4-syntax-table)
  (defvar maplev-help-mode-syntax-table)
  (defvar maplev-history-list)
  (defvar maplev-mode-4-syntax-table)
  (defvar maplev-symbol-syntax-table)
  (defvar maplev-quote-not-string-syntax-table))

(declare-function maplev-reset-font-lock "maplev")
(declare-function maplev--re-search-backward "maplev-mint")
(declare-function maplev--re-search-forward "maplev-mint")
(declare-function maplev--string-to-name "maplev-utils")

;;}}}
;;{{{ Compatibility assignments

(eval-and-compile
  (if (not (boundp 'folding-mode)) (defvar folding-mode nil))
  (if (not (fboundp 'folding-open-buffer)) (defun folding-open-buffer ()))

  (defun event-window (event)
    "Return the window over which mouse EVENT occurred."
    (nth 0 (nth 1 event)))

  (defun event-point (event)
    "Return the character position of the mouse EVENT."
    (posn-point (event-start event))))

;;}}}
;;{{{ Release

(defun maplev-setup ()
  "If in `maplev-mode' also refontify the buffer."
  (cond ((memq major-mode '(maplev-mode maplev-pmaple-mode maplev-view-mode))
	 (set-syntax-table maplev-mode-syntax-table))
        ;; for consistency also maplev-help-mode
        ((eq major-mode 'maplev-help-mode)
         (set-syntax-table maplev-help-mode-syntax-table)))
  (when (eq major-mode 'maplev-mode)
    (maplev-reset-font-lock)))


;;}}}
;;{{{ Mark Maple procedures

(defun maplev--beginning-of-defun-pos-0 ()
  "Return character position of beginning of previous defun.
Return nil if search fails."
  (let ((regexp maplev--defun-begin-re)
        pos)
    (save-excursion
      (while
          (and (setq pos (maplev--re-search-backward regexp nil 'move))
               (if (looking-back "end\\s-+" (line-beginning-position))
                   (not (setq pos nil)))))
      pos)))

(defun maplev--beginning-of-defun-pos ()
  "Return character position of beginning of previous defun.
Return nil if search fails."
  (let ((regexp maplev--defun-begin-re)
        pos)
    (while
        (and (maplev--re-search-backward regexp nil 'move)
             (setq pos (point))
             (when (looking-back "end\\s-+" (line-beginning-position))
                 (not (setq pos nil)))))
    pos))



(defun maplev--end-of-defun-pos ()
  "Return character position of next end of defun."

  (let ((regexp maplev--defun-end-re-colon)
        pos)
    (save-excursion
      (and (setq pos
                 ;; Assign pos the position of the next end statement.
                 ;; Because point could be in the middle of the statement,
                 ;; first search forward, then backwards.  If the end position
                 ;; of the backwards search is past the original point (orig),
                 ;; then use it, otherwise use the end position of the forward search.
                 (let* ((orig (point))
                        (end (maplev--re-search-forward regexp nil 'move)))
                   (if end (goto-char (match-beginning 0)))
                   (or (and (maplev--re-search-backward regexp nil t)
                            (> (setq pos (match-end 0)) orig)
                            pos)
                       end)))
           pos))))

;; (goto-char (maplev--end-of-defun-pos))
;; foo := proc() proc() end proc; end proc;


(defun maplev--beginning-of-defun ()
  "Move point backwards to the beginning of the current defun.
The defun is a Maple procedure or module.  The beginning is the first
character of the keyword.  Complete end-statements are required."
  (interactive)
  (let ((count 0)
	(regex (concat "\\_<\\(?:\\(proc\\|module\\)"   ; 1
		       "\\|\\(do\\|if\\|try\\|use\\)"   ; 2
		       "\\|\\(end\\|fi\\|od\\|until\\)" ; 3
		       "\\)\\_>"))
	(state (syntax-ppss))
	(start (point)))
    (unless (or (nth 3 state)
    		(nth 4 state)
    		(not (looking-at maplev-partial-end-defun-re)))
      ;; Handle point in keywords by repositioning point
      ;; so algorithm correctly handles them.
      (goto-char (match-end 0))
      (if (looking-back "\\_<end\\s-\\(proc\\|module\\)?" nil)
    	  ;; point is in ending keywords.  Move before them.
    	  (goto-char (match-beginning 0))
	(if (not (looking-back "\\_<\\(?:proc\\|module\\)" nil))
	    (goto-char start)
	  (when (save-excursion
		  (goto-char start)
		  (looking-at " *\\(proc\\|module\\)"))
	    (goto-char start)))))
    (while (and (>= count 0)
		(re-search-backward regex nil 'move))
      (let ((state (syntax-ppss))) ; FIXME: speed this up
	(unless (or (nth 3 state)  ; string/quoted
		    (nth 4 state)) ; comment
	  (if (and (match-string-no-properties 3)
                   (not (looking-at "end\\s-+proc")))
	      (setq count (1+ count))
	    (unless (looking-back "end\\s-+" nil)
	      (if (or (match-string-no-properties 1)
		      (> count 0))
		  (setq count (1- count))))))))))



(defun maplev--end-of-defun ()
  "Move point forward to the end of the current defun.
THIS ASSUMES EACH END STATEMENT IS FOLLOWED BY AN APPROPRIATE KEYWORD."

  ;; To handle short end-statements, the search must be from the
  ;; beginning of the procedure, otherwise there is no way to tell when
  ;; the procedure has ended.  This currently ignores that by assuming
  ;; long end-statements are used.

  ;; This algorithm assumes a proc/module is bounded by its keywords.
  ;; Consider
  ;;
  ;;   foo := proc() ... end proc;
  ;;
  ;; If point is in "foo", it is considered outside the proc body
  ;; so moving to the end will move point to end of containing proc
  ;; (or end of file).

  (interactive)
  (let ((count 0)
        (regex (if current-prefix-arg
                   "\\_<\\(proc\\)\\|end\\(?:\\s-+proc\\)?\\_>"
                 "\\_<end\\(?:\\s-+proc\\)?\\_>"))
	(state (syntax-ppss))
	(start (point)))
    (unless (or (nth 3 state) ; non-nil = inside a string
		(nth 4 state) ; non-nil = inside a comment
		(not (looking-at maplev-partial-end-defun-re)))
      ;; Handle point in keywords by repositioning point
      ;; so algorithm correctly handles them.
      (goto-char (match-end 0))
      (if (looking-back "\\_<end\\s-+\\(proc\\|module\\)" nil)
	  ;; point is in ending keywords.  Move before them.
	  (goto-char (match-beginning 0))
	(goto-char start)
	;; point is at initial character of beginning keyword; move forward
	(if (looking-at "proc\\|module")
	    (goto-char (match-end 0)))))
    ;; Standard algorithm
    (while (and (>= count 0)
		(re-search-forward regex nil 'move))
      (setq state (parse-partial-sexp start (point) nil nil state)
	    start (point))
      (unless (or (nth 3 state)  ; string/quoted
		  (nth 4 state)) ; comment
        (unless (looking-at (rx (1+ space (or "do" "if") symbol-end)))
	  (setq count (if (and current-prefix-arg
                              (match-string-no-properties 1))
			  (1+ count)
		        (1- count))))))))  ; maplev--end-of-defun

;; (maplev--end-of-defun)
;; (re-search-forward  "\\(\\_<end\\>\\s-+\\)\\_<\\(?:proc\\|module\\)\\_>")
;; (re-search-forward "\\_<end\\_>" nil 'move)
;; proc() end proc;

(defun maplev-beginning-of-defun ()
  "Move point backward to the beginning of defun."
  (interactive)
  (goto-char (or (maplev--beginning-of-defun-pos)
                 (point-min))))

(defun maplev-end-of-defun ()
  "Move point forward to the end of defun."
  (interactive)
  (goto-char (or (maplev--end-of-defun-pos)
                 (point-max))))

(defun maplev-mark-defun ()
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point.
Return non-nil unless something dumb happens."
  (interactive)
  (push-mark (point) 'nomsg)
  ;;(beginning-of-line)
  (with-syntax-table maplev-symbol-syntax-table
    (if (looking-at maplev--defun-begin-re) (goto-char (match-end 0)))
    (let ((count 1) ; decrement for each end statement, increment for each proc
	  (regexp (concat "\\(" maplev--defun-begin-re "\\)\\|\\(?:" maplev--defun-end-re-colon "\\)"))
	  (o-point (point)) ; original point
	  (p-point (point)) ; point at which state is valid
	  (state (parse-partial-sexp (point-min) (point)))) ; FIXME, reuse saved state
      ;; move to end of current procedure, using count to skip over local procedure assignments.
      (while (and (/= count 0)
		  (re-search-forward regexp nil 'move))
	(setq state (parse-partial-sexp p-point (point) nil nil state)
	      p-point (point))
	(unless (or (nth 3 state) ; string/quote
		    (nth 4 state)) ; comment
          ;; update count: increment if the match was the begining of a procedure, decrement otherwise
	  (setq count (+ count (if (match-beginning 1) 1 -1)))))
      (forward-line)
      (if (/= count 0)
	  ;; at bottom of buffer without finding closing mark
	  (progn
	    (goto-char o-point)
	    (when (setq o-point (maplev--end-of-defun-pos))
	      (when (maplev--beginning-of-defun-pos)
		(push-mark o-point nil t))))
	;; at end of procedure
	(push-mark (point) 'nomsg t) ; set mark after end of current procedure.
	(when (re-search-backward maplev--defun-end-re-colon nil 'move)
	  (setq count -1)
	  (while (and (/= count 0)
		      (re-search-backward regexp nil 'move))
	    ;; TBD: rewrite to avoid parsing from point-min
	    (setq state (parse-partial-sexp (point-min) (point) nil nil state))
	    (unless (or (nth 3 state) ; string/quote
			(nth 4 state)) ; comment
	      (setq count (+ count (if (match-beginning 1) 1 -1)))))
	  (zerop count))))))

(defun maplev-current-defun ()
  "Return a list with buffer positions of begin and end of current defun.
Retun nil if current defun not found."
  (save-mark-and-excursion
    (when (maplev-mark-defun)
      (list (point) (mark)))))

(defun maplev-narrow-to-defun ()
  "Make text outside current defun invisible."
  (interactive)
  (widen)
  (let ((reg (maplev-current-defun)))
    (when reg
      (narrow-to-region (car reg) (nth 1 reg)))))


(defun maplev-what-proc (&optional nodisplay)
  "Display the name of the current procedure.
If optional NODISPLAY is non-nil, just return the name."
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (maplev-beginning-of-defun)
      (re-search-backward maplev--assignment-re)
      (let ((proc (match-string-no-properties 1)))
      (if nodisplay
          proc
        (message proc))))))


;;}}}
;;{{{ Select Maple identifiers

(defun maplev--ident-around-point (&optional default)
  "Return the identifier around the point as a string.
If it is empty use DEFAULT.
If choice is empty, an error is signaled, unless DEFAULT equals \"\" or t."
  ;; If point is in a string enclosed by backquotes,
  ;; we take the whole string including the backquotes.
  (let* ((state (parse-partial-sexp (point-min)
                                    (point)))
         (choice (if (equal ?` (nth 3 state))
                     ;; inside a backquoted symbol
                     (buffer-substring-no-properties
                      (nth 8 state)
                      (save-excursion (goto-char (nth 8 state)) ; goto start of string
				      (condition-case nil
					  (forward-sexp 1)
					(error (error "Unterminated quoted symbol")))
				      (point)))
		   (with-syntax-table maplev-quote-not-string-syntax-table
		     (current-word)))))
    (cond
     ((and choice (string-match "^`.*[^`]$" choice))
      (save-excursion
	(setq choice (and (looking-at (concat " *\\(" maplev--quoted-name-re "\\)"))
			  (match-string-no-properties 1)))))
     ((and choice (string-match "^[^`].*`$" choice))
      (save-excursion
	(setq choice (and (looking-back (concat "\\(" maplev--quoted-name-re "\\) *")
					(line-beginning-position))
			  (match-string-no-properties 1))))))
    (cond
     (choice choice)
     ((stringp default) default)
     (default "")
     (t (error "Empty choice")))))

(defun maplev-ident-around-point-interactive (prompt &optional default complete)
  "Request Maple identifier in minibuffer, using PROMPT.
Default is identifier around point.  If it is empty use DEFAULT.
Minibuffer completion is used if COMPLETE is non-nil."
  ;; Suppress error message
  (let* ((enable-recursive-minibuffers t)
        (ident (maplev--ident-around-point (or default t)))
	(prompt (concat prompt (unless (string-equal ident "")
                                  (concat " (default " ident ")"))
                         ": "))
	(choice (if complete
		    (completing-read prompt 'maplev--completion
				     nil nil nil maplev-history-list ident)
		  (read-string prompt nil maplev-history-list ident))))
    ;; Are there situations where we want to suppress the error message??
    (if (string-equal choice "")
        (error "Empty choice")
      choice)))

;;}}}

(provide 'maplev-common)

;;; maplev-common.el ends here


