;;; maplev-view.el --- mode for displaying Maple procedures

;;; Commentary:
;; 

;;; Code:
;;

(require 'comint)
(require 'maplev-compat)
(require 'maplev-config)

(eval-when-compile
  (defvar maplev-config-default)
  (defvar maplev--process-item)
  (defvar maplev-builtin-functions)
  (defvar maplev-help-mode-map)
  (defvar maplev-mode-syntax-table))

(declare-function event-point "maplev-common")
(declare-function event-window "maplev-common")
(declare-function maplev--cleanup-buffer "maplev-cmaple")
(declare-function maplev--cmaple-process "maplev-cmaple")
(declare-function maplev--ident-around-point "maplev-common")
(declare-function maplev-history--stack-current "maplev-history")
(declare-function maplev-history--stack-process "maplev-history")
(declare-function maplev-history-clear "maplev-history")
(declare-function maplev-ident-around-point-interactive "maplev-common")
(declare-function maplev-indent-buffer "maplev-indent")
(declare-function maplev-mode "maplev")
(declare-function maplev-reset-font-lock "maplev")
(declare-function maplev-cmaple-direct "maplev-cmaple")


;;{{{ mode map

(defvar maplev-view-mode-map
  (let ((map (copy-keymap maplev-help-mode-map)))
    ;; remove P (parent) key-binding
    (define-key map [?P] nil)
    ;; (define-key map [?v] 'maplev-view-toggle-view)
    (define-key map [?g] 'maplev-view-goto-source)
    map)
  "Keymap used in `maplev-view-mode'.")

;;}}}
;;{{{ mode definition

(define-derived-mode maplev-view-mode fundamental-mode
  "Major mode for displaying the source code of Maple procedures.
\\{maplev-view-mode-map}"
  :syntax-table maplev-mode-syntax-table
  :abbrev-table nil
  
  (set (make-local-variable 'maplev--process-item) #'maplev--proc-process)

  (make-local-variable 'maplev-history--stack) ; set up the stack
  (maplev-history-clear)

  ;; Mint support
  (make-local-variable 'maplev-mint--code-beginning)
  (make-local-variable 'maplev-mint--code-end)

  ;; font-lock support
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-maximum-decoration)
  (maplev-reset-font-lock)

  (setq buffer-read-only t))

(defun maplev-view-setup (&optional config)
  "Unless already assigned, set `major-mode' to `maplev-view-mode'.
Optional CONFIG is an object of type `maplev-config-class'."
  (unless (eq major-mode 'maplev-view-mode)
    (maplev-view-mode))
  (setq maplev-config (or config maplev-config maplev-config-default)
	mode-name (format "Maple-View: %s" (slot-value maplev-config 'maple))))

;;}}}
;;{{{ mode functions


(defun maplev--proc-buffer ()
  "Return the name of the Maple procedure listing buffer."
  (format "Maple proc (%s)" (slot-value maplev-config 'maple)))


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
    (let ((config maplev-config))
      (with-current-buffer (get-buffer-create (maplev--proc-buffer))
	(maplev-view-setup config)
	(maplev-history--stack-process proc hide)))))

(defun maplev--proc-process (proc)
  "Display the Maple procedure PROC \(a string\) in `maplev--proc-buffer'."
  (let ((process (maplev--cmaple-process)))
    (set-process-filter process 'maplev-view-filter)
    (set-buffer (maplev--proc-buffer))
    (setq mode-line-buffer-identification (format "%-12s" proc))
    (let (buffer-read-only)
      (delete-region (point-min) (point-max))
      (goto-char (point-min)))
    (comint-simple-send process (format "maplev:-Print(\"%s\"):%c" proc 0))))

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
	(beginning-of-line)
	(if (not (looking-at "(\\*\\*) $"))
	    (goto-char (point-max))
	  (delete-region (point) (point-max))
	  (maplev-view-cleanup-buffer))))))

(defun maplev-view-cleanup-buffer ()
  "Cleanup Maple procedure listings."
  (save-excursion
    ;; Delete multiple spaces.
    (goto-char (point-min))
    (while (re-search-forward "[ \t][ \t]+" nil t)
      (replace-match " "))
    ;; terminate with `;'
    (goto-char (point-max))
    (delete-region (line-beginning-position) (point-max))
    )
  (maplev-indent-buffer)
  (set-buffer-modified-p nil)
  (font-lock-ensure))

;;}}}

;;{{{ view source

(defun maplev-view-toggle-view ()
  "Toggle the view of the code between showstat and file."
  (interactive))

(defun maplev-view--show-source (proc)
  "Display the source of Maple procedure PROC."
  (let ((file-line (maplev-view--get-source-and-line proc)))
    (when file-line
      (let ((file (car file-line))
	    (line (cdr file-line)))
	(delete-region (point-min) (point-max))
	(insert-file-contents file)
	(goto-char (point-min))
	(forward-line (1- line))))))
    
(defun maplev-view-goto-source ()
  "Goto the source of the current Maple procedure."
  (interactive "P")
  (let* ((proc (maplev-history--stack-current))
	 (file-line (maplev-view--get-source-and-line proc)))
    (if file-line
	(let* ((file (car file-line))
	       (line (cdr file-line))
	       (base file)
	       mroot file-exists)
	  (when (and (= (aref file 0) ?>)
		     (setq mroot (getenv "MAPLE_ROOT"))
		     (setq base (substring file 1)))
	    (setq file (concat mroot "/" base)))

	  (if (not (file-exists-p file))
	      (error "File %s does not exist" file)
	    ;; Open the file
	    (find-file file)
	    (if (eq major-mode 'fundamental-mode)
		(maplev-mode))
	    (goto-char (point-min))
	    (forward-line (1- line)))
	  (error "No line-info data for %s" proc)))))

(defun maplev-view--get-source-and-line (proc)
  "Return the filename and line number of the source for Maple procedure PROC.
If found, they are returned as a cons-cell \(file \. line\),
otherwise nil is returned."
  (let* ((cmd (format "lprint(maplev:-GetSource(\"%s\")):" proc))
	 (res (maplev-cmaple-direct cmd 'delete))
	  file line)
    (when (and (not (string= res "NULL"))
	       (string-match "^\\[\"\\([^\"]*\\)\", \\([0-9]+\\)" res))
      (setq file (match-string 1 res)
	    line (string-to-number (match-string 2 res)))
      (cons file line))))

;;}}}

(provide 'maplev-view)

;;; maplev-view.el ends here
