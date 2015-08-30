;;; maplev-cmaple.el --- Communicate with Maple process

;;; Commentary:
;; 

;; Define the functions used for communicating with the command line
;; Maple process.
;;
;; A useful feature is having independent Maple processes associated
;; with particular (source) buffers.  Doing so will require rewriting
;; the access control, however, it should result in a more robust
;; design.  Is it worth it? 
;;
;; One method to accomplish this is the following:
;;
;;  - Create a (source) buffer-local variable that stores the process.
;;  - Create an (output) buffer-local flag variable that stores the lock status.
;;
;; To check whether the process is locked, make the output buffer the
;; current buffer and check its flag variable.  When a second source
;; buffer (first) requires a Maple process, the user should be queried
;; (dependent on a configuration variation)  whether it should use an
;; existing Maple process, provided it is of the proper release.
;; Independent Maple output buffers should be numbered sequentially.
;;
;; A difficulty, or at least a nusiance, is handling the help and proc
;; modes.  Ideally all source buffers that have the same Maple release
;; would use a common help or proc buffer.  However, because proc may
;; depend on the state of Maple, its buffer must be associated with a
;; specific Maple process.  The straightforward solution is to have a
;; separate help or proc buffer associated with each independent Maple
;; process.  It leads to more buffers than I'd like.  

;;; Code:

(require 'comint)

(eval-when-compile
  (defvar maplev-cmaple-echoes-flag)
  (defvar maplev-cmaple-end-notice)
  (defvar maplev-executable-alist)
  (defvar maplev-include-path)
  (defvar maplev-mint-error-level)
  (defvar maplev-release)
  (defvar maplev-start-options)
  (defvar maplev-startup-directory))

(declare-function maplev-mint-region "maplev-mint")
(declare-function maplev-current-defun "maplev-common")
(declare-function maplev-set-release "maplev-common")

(defconst maplev-cmaple-prompt "> "
  "String inserted as a prompt.")

;;{{{ functions

(defun maplev--cmaple-buffer ()
  "Return the name of the Maple cmaple buffer."
  (format "Maple %s" maplev-release))

(defun maplev--cmaple-process ()
  "Return the cmaple process associated with the current buffer.
Start one, if necessary."
  (let ((process (get-buffer-process (maplev--cmaple-buffer))))
    (if (and process (eq (process-status process) 'run))
        process
      (maplev-cmaple--start-process))))

(defun maplev-cmaple--start-process ()
  "Start a cmaple process associated with the current buffer.
Return the process.  If such a process already exists, kill it and
restart it."
  (let* ((release maplev-release)
	 (executable (cdr (assoc release maplev-executable-alist)))
         (cmaple (nth 0 executable))
         (inifile (nth 1 executable))
         (buffer (get-buffer-create (maplev--cmaple-buffer)))
         (process (get-buffer-process buffer))
	 (include-path maplev-include-path)
         ;; Just testing this.  Is there an advantage to a PTY process?
         (process-connection-type 'pty)) 
    (with-current-buffer buffer
      (message "Starting Maple %s..." release)
      (if process (delete-process process))
      (if maplev-startup-directory
          (cd (expand-file-name maplev-startup-directory)))
      (set-process-filter
       ;; `apply' is used because `maplev-start-options' is a list.
       (setq process (apply 'start-process
                            (format "Maple %s" release)
                            buffer
                            cmaple
                            (append (and inifile (list "-i" inifile))
				   '("-c maplev:-Setup()")
                                    maplev-start-options ;; add include path to argument list
                                    (and include-path
                                         (list (concat "-I " 
                                                       (mapconcat 'identity include-path ",")))))))
       'maplev--cmaple-filter)
      (maplev-cmaple-mode release)
      (maplev-cmaple--lock-access t)
      ;; (comint-simple-send process init-code
      (maplev-cmaple--send-end-notice process)
      ;; Wait until cmaple is unlocked, that is, it has responded.
      ;; The time step, 100 milliseconds, should be customizable, some OSs
      ;; do not support fractions of seconds.
      ;; (while (maplev-cmaple--locked-p) (maplev--short-delay))
      (maplev-cmaple--wait)
      (message "Maple %s started" release)
      process)))

;; Access control

;; JR: Are the lines marked "hieida" the original or his suggested
;; correction?  I don't see the point of using a fixed symbol,
;; maplev-release as the property in which to store the lock status.
;; Using the value of maplev-release makes sense.  Alas, I no longer
;; have his email.  A better way to handle this might be to attach the
;; property to a buffer local variable.  However, I don't think that
;; that is possible.  Possibly the correct technique is to create a
;; flag variable that is local to the Maple output buffer and assign
;; to it.

(defun maplev-cmaple--lock-access (&optional no-error)
  "Lock access to cmaple.
If access is already locked, generate an error
unless optional arg NO-ERROR is non-nil."
  (if (and (not no-error) (maplev-cmaple--locked-p))
      (error "Maple busy")
    (put 'maplev-cmaple-state 'maplev-release 'locked)))

(defun maplev-cmaple--unlock-access ()
  "Unlock access to cmaple.
Interactively use \\[maplev-cmaple-interrupt]."
  (put 'maplev-cmaple-state 'maplev-release nil))

(defun maplev-cmaple--locked-p ()
  "Return non-nil if the Maple process is locked."
  (eq (get 'maplev-cmaple-state 'maplev-release) 'locked))

(defun maplev-cmaple-status ()
  "Status of Maple process."
  (interactive)
  (let ((status (get 'maplev-cmaple-state 'maplev-release)))
    (message "Maple %s %s" maplev-release
             (cond ((eq status 'locked) "locked")
                   ((not status) "unlocked")
                   (status)))))

(defun maplev-cmaple--wait (&optional max-cnt no-err)
  "Wait for cmaple to become available.  
If optional argument MAX-CNT is non-nil, wait at most that many
seconds; otherwise wait indefinitely.  If optional argument NO-ERR is
non-nil do not generate an error if time-out occurs."
  (with-temp-message "Maple busy, waiting..."
    (let ((cnt (* 10 (or max-cnt 0))))
      (while (and (maplev-cmaple--locked-p)
                  (or (null max-cnt)
                      (< 0 (setq cnt (1- cnt)))))
        ;; Should sit-for be used instead?  It permits interrupting
        ;; via user input (keystrokes).
        (sleep-for 0.1))
      (and (not no-err)
           (maplev-cmaple--locked-p)
           (error "Maple busy")))))

;; Functions that send stuff to cmaple

(defun maplev-cmaple-send ()
  "Send input to Maple."
  (interactive)
  (let ((pmark (process-mark (maplev--cmaple-process)))
        (maplev-mint-info-level maplev-mint-error-level)
        (comint-input-sender (function maplev-cmaple--send-string)))
    ;; Only _new_ input is checked for typos, see comint-send-input.
    ;; We might need something smarter for comint-get-old-input.
    ;; Why does comint-send-input use (line-end-position) instead of
    ;; (point-max)?  To be consistent maplev-mint-region does the same.
    (if (or (< (point) (marker-position pmark))
            (equal 0 (maplev-mint-region pmark (line-end-position))))
        (comint-send-input))))

(defun maplev-cmaple--send-string (process string)
  "Send STRING to the cmaple process PROCESS."
  (maplev-cmaple--lock-access)
  (set-process-filter process 'maplev--cmaple-filter)
  (comint-simple-send process string)
  (maplev-cmaple--send-end-notice process))

(defun maplev-cmaple-send-region (beg end)
  "Send the region from BEG to END to cmaple.
If called interactively use the marked region.
If called with a prefix the cmaple buffer is first cleared."
  (interactive "r")
  (let ((maplev-mint-info-level maplev-mint-error-level)) ;; TODO: Change to -S for syntax only!
    (when (equal 0 (maplev-mint-region beg end))
      (and current-prefix-arg (maplev-cmaple--clear-buffer))
      (maplev-cmaple--send-string (maplev--cmaple-process)
                                  (buffer-substring-no-properties beg end)))))

(defun maplev-cmaple-send-line ()
  "Send the current line to cmaple."
  (interactive)
  (maplev-cmaple-send-region (line-beginning-position) (line-end-position)))

(defun maplev-cmaple-send-buffer ()
  "Send the buffer to cmaple."
  (interactive)
  (maplev-cmaple-send-region (point-min) (point-max)))

(defun maplev-cmaple-send-procedure ()
  "Send the current procedure to cmaple."
  (interactive)
  (apply 'maplev-cmaple-send-region (maplev-current-defun)))

(defun maplev-cmaple-direct (input &optional delete)
  "Send the string INPUT to cmaple and return the output.
If optional argument DELETE is non-nil, delete the echoed Maple input
from the output buffer.  This is a very simple function, it assumes
that the input consists of one line and the output is on the following line."
  ;; This may not work on a Windows box; there, the input is not echoed
  ;; to the output buffer.
  (interactive)
  ;; (while (maplev-cmaple--locked-p) (maplev--short-delay))
  (maplev-cmaple--wait)
  (save-current-buffer
    (let ((proc (maplev--cmaple-process))) ; ensure Maple is started
      (set-buffer (maplev--cmaple-buffer))
      (save-restriction
        (narrow-to-region (point-max) (point-max))
        (maplev-cmaple--send-string proc input)
        ;; (while (maplev-cmaple--locked-p) (maplev--short-delay))
        (maplev-cmaple--wait)
        (goto-char (point-min))
        (forward-line)
        (let ((output (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
          (if delete
              (delete-region (point-min) (point-max)))
          output)))))
      
(defun maplev-cmaple--send-end-notice (process)
  "Send a command to PROCESS \(cmaple\) to print `maplev-cmaple-end-notice'."
  (comint-simple-send process (concat "lprint(" maplev-cmaple-end-notice ");")))

(defun maplev-cmaple--ready (process)
  "Return t if PROCESS \(cmaple\) is ready for new input, nil otherwise.
Remove `maplev-cmaple-end-notice' from the current buffer.
Reset the filter for PROCESS \(cmaple\) and unlock access."
  (let (case-fold-search)
    (save-excursion
      (when (re-search-backward
             (concat maplev-cmaple-end-notice "\n") nil t)
        (delete-region (match-beginning 0) (match-end 0))
        (when (and maplev-cmaple-echoes-flag
                   (re-search-backward
                    (concat "lprint(" maplev-cmaple-end-notice ");\n")
                    nil t))
          (delete-region (match-beginning 0) (match-end 0)))
        (maplev--cleanup-buffer)
        (set-process-filter process 'maplev--cmaple-filter)
        (maplev-cmaple--unlock-access)
        t))))

(defun maplev-cmaple-interrupt ()
  "Interrupt Maple."
  (interactive)
  (let ((process (get-buffer-process (maplev--cmaple-buffer))))
    (message "Interrupt process %s" (process-name process))
    (interrupt-process process)
    (maplev-cmaple--unlock-access)))

(defun maplev-cmaple-kill ()
  "Kill Maple."
  (interactive)
  (let ((process (get-buffer-process (maplev--cmaple-buffer))))
    (message "Kill process %s" (process-name process))
    (kill-process process)))

(defun maplev-cmaple--clear-buffer ()
  "Clear the contents of the cmaple buffer."
  (with-current-buffer (maplev--cmaple-buffer)
    (delete-region (point-min) (point-max))))
                 

(defun maplev-cmaple-pop-to-buffer (&optional release)
  "Pop up a buffer with command line Maple.  Start Maple, if necessary.
Optional arg RELEASE defaults to `maplev-release'."
  (interactive
   (list (if current-prefix-arg
             (completing-read "Maple release: "
                              (mapcar #'(lambda (item) (list (car item)))
                                      maplev-executable-alist)
                              nil t))))
  (unless release (setq release maplev-release))
  (let ((maplev-release release))
    (maplev--cmaple-process)
    (pop-to-buffer (maplev--cmaple-buffer))
    (goto-char (point-max))))

(defalias 'cmaple 'maplev-cmaple-pop-to-buffer)

(defun maplev--cmaple-filter (process string)
  "Send the Maple output to the Maple buffer.
PROCESS is the Maple process, STRING its output."
  (with-current-buffer (process-buffer process)
    (let ((pmark (process-mark process)))
      (save-excursion
        (save-restriction
          (goto-char pmark)
          (narrow-to-region (point) (point))
          (insert string)
          (maplev--cleanup-buffer)
          (goto-char (point-max))
          (set-marker pmark (point)))
        (when (maplev-cmaple--ready process)
          (insert maplev-cmaple-prompt)
          (set-marker pmark (point))))
      (goto-char pmark))))

(defun maplev--cleanup-buffer ()
  "Remove over-striking and underlining from the current buffer."
  (goto-char (point-min))
  (while (re-search-forward "\e\\[[0-9;]+m" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\r+" nil t) (replace-match "\n")))

;;}}}
;;{{{ mode map

(defvar maplev-cmaple-map nil
  "Keymap used in Maple cmaple mode.")

(unless maplev-cmaple-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map [(return)]                'maplev-cmaple-send)
    (define-key map [(control c) (control c)] 'maplev-cmaple-interrupt)
    (define-key map [?\?]                     'maplev-help-at-point)
    (define-key map [(control ?\?)]           'maplev-help-at-point)
    (define-key map [(meta ?\?)]              'maplev-view-at-point)
    (define-key map [(meta tab)]              'maplev-complete-symbol)
    (define-key map [(control a)]             'comint-bol)

    (when (fboundp 'x-get-cut-buffer)
      ;; These two bindings are needed only under linux / unix
      (define-key map [(meta control y)]        'maplev-insert-cut-buffer)
      ;; mouse button bindings
      (define-key map [(control meta mouse-2)]  'maplev-mouse-yank-cut-buffer))
    
    (define-key map [(shift mouse-2)]         'maplev-help-follow-mouse)
    (define-key map [(control shift mouse-2)] 'maplev-help-follow-mouse)
    (define-key map [(meta shift mouse-2)]    'maplev-view-follow-mouse)

    ;; in comint-mode-map of emacs 21, `C-c C-s' is bound to comint-write-output.
    ;; Remove it so that it can be used as a prefix key to switch buffers.
    (define-key map [(control c) (control s)]     nil)
    (define-key map [(control c) (control s) ?h] 'maplev-switch-buffer-help)
    (define-key map [(control c) (control s) ?l] 'maplev-switch-buffer-proc)
    (define-key map [(shift return)]             'newline)
    (setq maplev-cmaple-map map)))

;;}}}
;;{{{ mode

(defconst maplev-input-line-keyword
  `((,(concat "^" maplev-cmaple-prompt ".*$") . maplev-input-face))
  "Keyword for font locking input lines in cmaple mode.")

(defun maplev-cmaple-mode (&optional release)
  "Major mode for interacting with cmaple.
RELEASE is an id in `maplev-executable-alist'; if omitted the
first id is used.  This mode has the same commands as
`comint-mode' plus some additional commands for interacting with
cmaple.

\\{maplev-cmaple-map}"
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp (concat "^\\(" maplev-cmaple-prompt "\\)+ *")
        ;; GNU Emacs 21
        comint-eol-on-send t
        major-mode 'maplev-cmaple-mode
        mode-name "Maple")
  (if (< emacs-major-version 22)
      (with-no-warnings
	(setq comint-use-prompt-regexp-instead-of-fields t))
    (setq comint-use-prompt-regexp t))

  ;; Mint support
  (make-local-variable 'maplev-mint--code-beginning)
  (make-local-variable 'maplev-mint--code-end)

  (maplev-set-release release)
  (use-local-map maplev-cmaple-map)
  (set (make-local-variable 'font-lock-defaults)
       '(maplev-input-line-keyword))
  (set (make-local-variable 'comint-process-echoes)
       maplev-cmaple-echoes-flag)
  (make-local-variable 'maplev-cmaple-prompt)
  (font-lock-mode 1)
  (run-hooks 'maplev-cmaple-mode-hook))

;;}}}

(provide 'maplev-cmaple)

;;; maplev-cmaple.el ends here
