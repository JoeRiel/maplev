;;; maplev-history.el --- History mechanism for help and proc modes

;;; Commentary:
;; 

;; History of history.
;;
;; Originally this structure was implemented as a browsable stack.
;; New entries were always inserted on the top.  The usage,
;; however, seemed confusing.  Bringing up a new node while browsing
;; the stack would move you to the top of the stack, away from where
;; you were.
;;
;; The new design inserts entries where you are at.  An interesting
;; modification, not implemented (yet) would be to make this a
;; rolodex, that is, a ring rather than a stack.

;;; Code:
;;

(require 'maplev-custom)

(defvar maplev-history-list nil
  "History list used by maplev.")

(defvar maplev-history--stack nil
  "Stack variable used for the history mechanism.
It is local to the `maplev-help-mode' and `maplev-view-mode' buffers.")

(defvar maplev--process-item nil
  "The name of a function that processes items on `maplev-history--stack'.
It is local to the `maplev-help-mode' and `maplev-view-mode' buffers.")

;;{{{ Module

;; Implement a stack-like structure for providing a history mechanism
;; for the Help and Proc modes.  The stack is a list.  The car of the
;; list is an integer that indexes a particular element in the list;
;; it is used when scrolling through the stack.

(defvar maplev-history--stack nil
  "List containing history of previous `commands'.
The car of the list is an integer that indexes a particular element in
the list, it is used to scroll through the stack.  This is a
buffer-local variable associated with the Maple Help and Maple Proc
output buffers.")

(defun maplev-history--stack-insert (item)
  "Put ITEM into `maplev-history--stack'."
  (let ((pos (car maplev-history--stack)))
    (setcdr (nthcdr pos maplev-history--stack)
            (cons item (nthcdr (1+ pos) maplev-history--stack)))))

(defun maplev-history--stack-prev ()
  "Return the item on `maplev-history--stack' preceding the one last accessed.
If at the bottom of the stack return nil, otherwise increment the pointer."
  (let* ((pos (1+ (car maplev-history--stack)))
         (item (nth pos (cdr maplev-history--stack))))
    (when item
      (setcar maplev-history--stack pos)
      item)))

(defun maplev-history--stack-next ()
  "Return the item on `maplev-history--stack' following the one last accessed.
If at the top of the stack, return nil, otherwise decrement the pointer."
  (let ((pos (1- (car maplev-history--stack))))
    (when (>= pos 0)
      (setcar maplev-history--stack pos)
      (nth pos (cdr maplev-history--stack)))))

(defun maplev-history--stack-top ()
  "Return the top item of `maplev-history--stack'.
Do not change the pointer."
  (nth 1 maplev-history--stack))

(defun maplev-history--stack-current ()
  "Return the currently accessed element of `maplev-history--stack'."
  (nth (car maplev-history--stack) (cdr maplev-history--stack)))

;;}}}
;;{{{ Commands

;;; The following commands process the history items.  The symbol
;;; `maplev--process-item' should be buffer local and assigned the
;;; name of the function that process the items.

(defsubst maplev--process-item-func (item)
  "Apply the function symbol `maplev--process-item' to ITEM."
  (if (stringp item)
      (funcall maplev--process-item item)
    (message "End of stack")))

(defun maplev-history-next-item ()
  "Process the next item on `maplev-history--stack'."
  (interactive)
  (maplev--process-item-func (maplev-history--stack-next)))

(defun maplev-history-prev-item ()
  "Process the previous item on `maplev-history--stack'."
  (interactive)
  (maplev--process-item-func (maplev-history--stack-prev)))

(defun maplev-history-redo-item ()
  "Process the current item on `maplev-history--stack'."
  (interactive)
  (maplev--process-item-func (maplev-history--stack-current)))

(defun maplev-history-delete-item ()
  "Delete current item from `maplev-history--stack'."
  (interactive)
  (when maplev-history--stack
    (let ((pos (car maplev-history--stack)))
      (setcdr (nthcdr pos maplev-history--stack)
              (nthcdr (+ 2 pos) maplev-history--stack))
      (unless (nth pos (cdr maplev-history--stack))
        (setcar maplev-history--stack (setq pos (1- pos))))
      (if (>= pos 0)
          (maplev--process-item-func (maplev-history--stack-current))
        (kill-buffer)))))

(defun maplev-history-clear ()
  "Assign `maplev-history--stack' an empty stack."
  (interactive)
  (setq maplev-history--stack (list 0)))

(defun maplev-history--stack-process (item &optional hide)
  "Insert ITEM into `maplev-history--stack' and process it.
Do not insert ITEM into the stack if it is already at the current
or following position.
If optional arg HIDE is non-nil do not display buffer."
  (let ((pos (car maplev-history--stack)))
    (unless (or (string= item (maplev-history--stack-current))
                (and (/= pos 0)
                     (string= item (nth pos maplev-history--stack))))
      (maplev-history--stack-insert item))
    (maplev--process-item-func item)
    (unless hide
      (let ((pop-up-frames maplev-pop-up-frames-flag))
        (display-buffer (current-buffer) nil t)))))

;;}}}



(provide 'maplev-history)

;;; maplev-history.el ends here
