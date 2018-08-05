;;; maplev-trace.el --- handle Maple trace output

;;; Commentary:

;; Maple tracing provides a convenient way to record the internal
;; execution of selected Maple procedures.  See ?trace.  When tracing,
;; Maple prints enter and exit markers for the traced procedures and
;; the result of each assignment during the execution.
;;
;; These Elisp functions use the embedded markers to indent the output
;; hierarchically so that one can see how deep a call is; this is
;; useful when debugging recursive calls.  For the output to be
;; sensible, interface(prettyprint=0) should be in effect.

;;; Code:

(defvar maplev-trace-indent 4
  "Variable used to set the indentation of each level.")

(defconst maplev--trace-re
  "^\\s-*\\(?:{--> \\(enter\\)\\|<-- \\(?:exit\\|\\(ERROR\\) in\\)\\) \\([^ ]*\\) \\(?:(now in \\([^)]*\\)\\)?"
"Regular expression that matches Maple trace output enter/exit markers.
Group 1 is assigned if this matches an enter marker.
Group 2, if assigned, matches ERROR.
Group 3 matches a procedure name.
Group 4, assigned if an exit or error; matches the procedure returned to.")

(defun maplev-trace-indent-region (beg end)
  "Hierarchically indent Maple trace output from BEG to END.
Use `maplev-trace-indent' as the indentation level.
Raise an error if the exit markers do not match the enter markers."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let ((col 0)                      ; column to indent to
	  (indent maplev-trace-indent) ; indent per level; local copy, for readability
	  (point (point))              ; point starting a block to indent
	  (end (set-marker (make-marker) end))) ; marker so it works with editing
      (while (re-search-forward maplev--trace-re end 'noerror)
	;; font-lock before indenting so positions are known
	(put-text-property (match-beginning 3) (match-end 3) 'face font-lock-function-name-face)
	(when (match-string-no-properties 4)
	  ;; exiting; font-lock the procedure we're returning to
	  (put-text-property (match-beginning 4) (match-end 4) 'face font-lock-function-name-face)
	  (if (match-string-no-properties 2)
	      ;; trapped an error, highlight it
	      (put-text-property (match-beginning 2) (match-end 2) 'face font-lock-warning-face)))
	;; indent region from point to current position
	(indent-region point (point) col)
	;; update col and adjust current line as needed
	(if (match-string-no-properties 1)
	    ;; enter marker
	    (setq col (+ col indent))
	  ;; exit marker
	  (if (zerop col)
	      (error "Too many exit markers")
	    (setq col (- col indent))
	    (indent-line-to col)))
	(end-of-line) ; move to end to skip the previous match
	(setq point (point)))
      (set-marker end nil)  ; clear the marker
      (unless (zerop col)
	(error "Missing %d exit markers" (/ col indent))))))

(defun maplev-trace-indent-buffer ()
    "Hierarchically indent Maple trace output in the buffer.
Use `maplev-trace-indent' as the indentation level."
  (interactive)
  (maplev-trace-indent-region (point-min) (point-max)))

(provide 'maplev-trace)

;;; maplev-trace.el ends here
