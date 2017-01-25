;;; maplev-common.el --- Common functions for maplev

;;; Commentary:
;; 

;;; Code:
;;

;;{{{ Suppress byte-compile warnings

(require 'maplev-custom)
(require 'maplev-re)

(eval-when-compile
  (defvar maplev-mode-syntax-table)
  (defvar maplev-mode-4-syntax-table)
  (defvar maplev-help-mode-syntax-table)
  (defvar maplev-history-list)
  (defvar maplev-mode-4-syntax-table)
  (defvar maplev-mode-syntax-table))

(declare-function maplev-reset-font-lock "maplev")
(declare-function maplev--re-search-backward "maplev-mint")
(declare-function maplev--re-search-forward "maplev-mint")
(declare-function maplev-safe-position "maplev-mint")
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
  (cond ((memq major-mode '(maplev-mode maplev-cmaple-mode maplev-view-mode))
	 (set-syntax-table maplev-mode-syntax-table))
        ;; for consistency also maplev-help-mode
        ((eq major-mode 'maplev-help-mode)
         (set-syntax-table maplev-help-mode-syntax-table)))
  (when (eq major-mode 'maplev-mode)
    (maplev-reset-font-lock)))


;;}}}
;;{{{ Mark Maple procedures

(defun maplev--beginning-of-defun-pos (&optional top n)
  "Return character position of beginning of previous defun.
If optional argument TOP is non-nil, search for top level defun.
With optional argument N, do it that many times.  Negative
argument -N means search forward to Nth preceding end of defun.
Return nil if search fails."
  (let ((regexp (if top maplev--top-defun-begin-re maplev--defun-begin-re))
        pos)
    (setq n (or n 1))
    (save-excursion
      (cond ((> n 0)
             (and (setq pos
                        ;; Assign pos the position of the previous beginning statement.
                        ;; Because point could be in the middle of the statement,
                        ;; first search backwards, then forwards.  If the beginning position
                        ;; of the forwards search is before the original point (orig),
                        ;; then use it, otherwise use the beginning position of the backwards search.
                        (let* ((orig (point))
                               (beg (maplev--re-search-backward regexp nil 'move)))
                          (if beg (goto-char (match-end 0)))
                          (or (and (maplev--re-search-forward regexp nil t)
                                   (< (setq pos (match-beginning 0)) orig)
                                   pos)
                              beg)))
                  ;; If n=1 then pos is the character position,
                  (if (= n 1)
                      pos
                    ;; otherwise, search backwards n-1 times.
                    ;; Because we are starting at the end of a defun,
                    ;; we don't have to do the backwards search.
                    (goto-char pos)
                    (maplev--re-search-backward regexp nil t (1- n)))))
            ((< n 0)
             (and (maplev--re-search-backward regexp nil t n)
                  (match-beginning 0)))
            ((point))))))

(defun maplev--end-of-defun-pos (&optional top n)
  "Return character position of next end of defun.
If optional argument TOP is non-nil, search for top level defun.
With optional argument N, do it that many times.  Negative
argument -N means search back to Nth preceding end of defun.
Return nil if search fails."

  ;; The search algorithm is asymmetric with respect to direction.
  ;; Searching backwards (-N) for an end of defun is easy, just search
  ;; and move to the end of the match.  Searching forward is more
  ;; complicated because point could lie within an end statement.

  (let ((regexp (if top maplev--top-defun-end-re maplev--defun-end-re))
        pos)
    (setq n (or n 1))
    (save-excursion
      (cond ((> n 0)
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
                  ;; If n=1 then pos is the character position,
                  (if (= n 1)
                      pos
                    ;; otherwise, search forward n-1 times.
                    ;; Because we are starting at the end of a defun,
                    ;; we don't have to do the backwards search.
                    (goto-char pos)
                    (maplev--re-search-forward regexp nil t (1- n)))))
            ((< n 0)
             (and (maplev--re-search-forward regexp nil t n)
                  (match-end 0)))
            ((point))))))

(defun maplev-beginning-of-defun (&optional n)
  "Move point backward to the beginning of defun.
With optional argument N, move to the beginning of the Nth
preceding defun.  Negative argument -N means move forward to the
end of the Nth following defun."
  (interactive)
  (setq n (or n 1))
  (goto-char (or (maplev--beginning-of-defun-pos nil n)
                 (if (> n 0) (point-min) (point-max)))))
       
(defun maplev-end-of-defun (&optional n)
  "Move point forward to the end of defun.
With optional argument N, move to the end of the Nth following
defun.  Negative argument -N means move backwards to the end of
the Nth preceding defun."
  (interactive)
  (setq n (or n 1))
  (goto-char (or (maplev--end-of-defun-pos nil n)
                 (if (> n 0) (point-max) (point-min)))))

(defun maplev-mark-defun ()
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point."
  (interactive)
  (push-mark)
  (beginning-of-line)
  (if (looking-at maplev--defun-begin-re) (goto-char (match-end 0)))
  (let ((count 1)
        (regexp (concat "\\(" maplev--defun-begin-re "\\)\\|\\(?:" maplev--defun-end-re "\\)")))
    (while (and (/= count 0)
                (re-search-forward regexp nil 'move))
      (setq count (+ count (if (match-beginning 1) 1 -1))))
    (forward-line)
    (push-mark (point) nil t)
    (when (= count 0)
      (goto-char (match-beginning 0))
      (setq count -1))
    (while (and (/= count 0)
                (re-search-backward regexp nil 'move))
      (setq count (+ count (if (match-beginning 1) 1 -1))))))

(defun maplev-current-defun ()
  "Return a list with buffer positions of begin and end of current defun."
  (save-excursion
    (maplev-mark-defun)
    (list (point) (mark))))

(defun maplev-narrow-to-defun ()
  "Make text outside current defun invisible."
  (interactive)
  (widen)
  (let ((reg (maplev-current-defun)))
    (narrow-to-region (car reg) (nth 1 reg))))


(defun maplev-what-proc (&optional nodisplay)
  "Display and return the name of the current procedure.
If optional NODISPLAY is non-nil, just return the string."
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (end-of-line)
      (maplev-beginning-of-defun)
      (re-search-forward maplev--assignment-re)
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
  (let* ((state (parse-partial-sexp (maplev-safe-position)
                                    (point)))
         (choice (if (equal ?` (nth 3 state))
                     ;; inside a string
                     (buffer-substring-no-properties
                      (nth 8 state)
                      (save-excursion (goto-char (nth 8 state))
                                      (forward-sexp 1) (point)))
                   (current-word))))
    (if (string-equal choice "")
        (cond ((stringp default)
               default)
              (default "")
              ((error "Empty choice")))
      choice)))

(defun maplev-ident-around-point-interactive (prompt &optional default complete)
  "Request Maple identifier in minibuffer, using PROMPT.
Default is identifier around point.  If it is empty use DEFAULT.
Minibuffer completion is used if COMPLETE is non-nil."
  ;; Suppress error message
  (if (not default) (setq default t))
  (let ((enable-recursive-minibuffers t)
        (ident (maplev--ident-around-point default))
        choice)
    (setq prompt (concat prompt (unless (string-equal ident "")
                                  (concat " (default " ident ")"))
                         ": ")
          choice (if complete
                     (completing-read prompt 'maplev--completion
                                      nil nil nil maplev-history-list ident)
                   (read-string prompt nil maplev-history-list ident)))
    ;; Are there situations where we want to suppress the error message??
    (if (string-equal choice "")
        (error "Empty choice"))
    (maplev--string-to-name choice)))

;;}}}

(provide 'maplev-common)

;;; maplev-common.el ends here
