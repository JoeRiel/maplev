;;; maplev-view.el --- mode for displaying Maple procedures

;;; Commentary:
;; 

;;; Code:
;;

(require 'comint)

(eval-when-compile
  (defvar maplev-builtin-functions)
  (defvar maplev--process-item)
  (defvar maplev-cmaple-echoes-flag)
  (defvar maplev-help-mode-map)
  (defvar maplev-release))

(declare-function event-point "maplev-common")
(declare-function event-window "maplev-common")
(declare-function maplev--cleanup-buffer "maplev-cmaple")
(declare-function maplev--cmaple-process "maplev-cmaple")
(declare-function maplev--ident-around-point "maplev-common")
(declare-function maplev-cmaple--lock-access "maplev-cmaple")
(declare-function maplev-cmaple--ready "maplev-cmaple")
(declare-function maplev-cmaple--send-end-notice "maplev-cmaple")
(declare-function maplev-history--stack-process "maplev-history")
(declare-function maplev-history-clear "maplev-history")
(declare-function maplev-ident-around-point-interactive "maplev-common")
(declare-function maplev-indent-buffer "maplev-indent")
(declare-function maplev-reset-font-lock "maplev")
(declare-function maplev-set-release "maplev-common")


;;{{{ mode map

;; The mode map for maplev-view-map is identical to that for
;; maplev-help-mode, with one exception: the parent function is not
;; needed, so its key is redefined to self-insert (which generates an
;; error, as does any other insertion, because the buffer if
;; read-only).

(defvar maplev-view-mode-map nil
  "Keymap used in `maplev-view-mode'.")

(unless maplev-view-mode-map
  (let ((map (copy-keymap maplev-help-mode-map)))
    (define-key map [?P] 'self-insert-command)
    (setq maplev-view-mode-map map)))

;;}}}
;;{{{ mode definition

(defun maplev-view-mode (&optional release)
  "Major mode for displaying the source code of Maple procedures.
RELEASE is an id in `maplev-executable-alist'; if nil, the
first id is used.

\\{maplev-view-mode-map}"
  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'maplev-view-mode) ;; needed by maplev-set-release
  (maplev-set-release release)
  (setq mode-name (format "Maple-View %s" maplev-release))
  (use-local-map maplev-view-mode-map)

  (set (make-local-variable 'maplev--process-item)
       (function maplev--proc-process))

  (make-local-variable 'maplev-history--stack) ; set up the stack
  (maplev-history-clear)

  ;; Mint support
  (make-local-variable 'maplev-mint--code-beginning)
  (make-local-variable 'maplev-mint--code-end)

  ;; font-lock support
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-maximum-decoration)
  (maplev-reset-font-lock)

  (setq buffer-read-only t)
  (run-hooks 'maplev-view-mode-hook))

;;}}}
;;{{{ functions


(defun maplev--proc-buffer ()
  "Return the name of the Maple procedure listing buffer."
  (format "Maple %s proc" maplev-release))


;;; Define functions for displaying a Maple procedure from the Maple
;;; library in a buffer.

(defun maplev-view-follow-mouse (click)
  "Display the Maple procedure at the mouse CLICK."
  (interactive "e")
  (set-buffer (window-buffer (event-window click)))
  (goto-char (event-point click))
  (maplev--proc-show-topic (maplev--ident-around-point)))

(defun maplev-view-at-point (proc)
  "Display the Maple procedure PROC.
Request procedure name in minibuffer, using identifier at point as default."
  (interactive (list (maplev-ident-around-point-interactive
                      "Maple procedure" nil t)))
  (maplev--proc-show-topic proc))

(defun maplev--proc-show-topic (proc &optional hide)
  "Display the Maple procedure PROC \(a string\).
Push PROC onto the local stack, unless it is already on the top.
If optional arg HIDE is non-nil do not display buffer."
  ;; Do not try to display builtin procedures.
  (if (member proc maplev-builtin-functions)
      (message "Procedure \`%s\' builtin." proc)
    (save-current-buffer
      (let ((release maplev-release)) ;; we switch buffers!
        (set-buffer (get-buffer-create (maplev--proc-buffer)))
        (unless (eq major-mode 'maplev-view-mode)
          (maplev-view-mode release))
        (maplev-history--stack-process proc hide)))))

(defun maplev--proc-process (proc)
  "Display the Maple procedure PROC \(a string\) in `maplev--proc-buffer'."
  (let ((process (maplev--cmaple-process)))
    (maplev-cmaple--lock-access)
    (set-process-filter process 'maplev-view-filter)
    (set-buffer (maplev--proc-buffer))
    (setq mode-line-buffer-identification (format "%-12s" proc))
    (let (buffer-read-only)
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      ;;(insert proc " := ")
      )
    (comint-simple-send process (format "maplev:-Print(\"%s\"):" proc))
    (maplev-cmaple--send-end-notice process)))

(defun maplev-view-filter (process string)
  "Pipe a Maple procedure listing into `maplev--proc-buffer'.
PROCESS calls this filter.  STRING is the Maple procedure."
  (with-current-buffer (maplev--proc-buffer)
    (save-excursion
      (let (buffer-read-only)
        (save-restriction
          (goto-char (point-max))
          (narrow-to-region (point) (point))
          (insert string)
          (maplev--cleanup-buffer))
        (goto-char (point-max))
        (if (maplev-cmaple--ready process)
            (maplev-view-cleanup-buffer))))))

(defun maplev-view-cleanup-buffer ()
  "Cleanup Maple procedure listings."
  (save-excursion
    (when maplev-cmaple-echoes-flag
      (goto-char (point-min))
      (if (re-search-forward "maplev:-Print(.+):\n" nil t)
          (delete-region (match-beginning 0) (match-end 0))))
    ;; Delete multiple spaces.
    (goto-char (point-min))
    (while (re-search-forward "[ \t][ \t]+" nil t)
      (replace-match " "))
    ;; terminate with `;'
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
;;    (insert ";")
    )
  (maplev-indent-buffer)
  (set-buffer-modified-p nil)
  (font-lock-fontify-buffer))

;;}}}

(provide 'maplev-view)

;;; maplev-view.el ends here
