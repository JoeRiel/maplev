;;; maplev-cmaple.el --- Communicate with Maple process

;;; Commentary:
;; 

;; Define the functions used for communicating with the command line
;; Maple process.  A change has been made for the 3.0 release;
;; communication is now done through pmaple, a binary executable
;; provided with this package.  Previously the cmaple command that
;; is part of Maple was used, however, it required handshaking to
;; communicate with Emacs and the result was never ideal.

;;; Code:

(require 'comint)
(require 'maplev-config)
(require 'maplev-custom)

(eval-when-compile
  (defvar maplev-mint-error-level)
  (defvar maplev-startup-directory)
  (defvar maplev-load-path))

(declare-function maplev-mint-region "maplev-mint")
(declare-function maplev-current-defun "maplev-common")

;;{{{ constants and variables

(defconst maplev-cmaple-prompt "(**) "
  "String inserted as prompt in Maple buffer.")


;;}}}
;;{{{ mode functions

(defun maplev--cmaple-buffer ()
  "Return the name of the cmaple buffer associated with the current buffer.
Use the buffer-local variable `maplev-config'."
  (concat "Maple" (and maplev-config
		       (format " (%s)" (slot-value maplev-config 'maple)))))

(defun maplev--cmaple-process ()
  "Return the cmaple process associated with the current buffer.
Start one, if necessary."
  (let ((process (get-buffer-process (maplev--cmaple-buffer))))
    (if (and process (eq (process-status process) 'run))
        process
      (maplev-cmaple--start-process))))

(defun maplev-cmaple-default-pmaple ()
  "Return the default path to the pmaple executable."
  (expand-file-name
   (let ((dir "~/maple/toolbox/maplev/"))
     (cond
      ((eq system-type 'gnu/linux)
       (concat dir "bin.X86_64_LINUX/pmaple"))
      ((eq system-type 'darwin)
       (concat dir "bin.APPLE_UNIVERSAL_OSX/pmaple"))
      ((member system-type '(windows-nt cygwin ms-dos))
       (concat dir "bin.X86_64_WINDOWS/pmaple.exe"))))))


(defun maplev-cmaple--process-environment ()
  "Return a list of strings of equations that define the process environment."
  (unless maplev-config
    (maplev-config))
  (let ((bindir   (slot-value maplev-config 'bindir))
	(mapledir (slot-value maplev-config 'mapledir)))
    (cond
     ((null bindir)
      (error "The :bindir slot of maplev-config is not assigned"))
     ((not (file-directory-p bindir))
      (error "The :bindir slot of maplev-config, `%s', does not exist" bindir))
     ((null mapledir)
      (error "The :mapledir slot of maplev-config is not assigned"))
     ((not (file-directory-p mapledir))
      (error "The :mapledir slot of maplev-config, `%s', does not exist" mapledir)))

    ;; create list of PATH=bindir MAPLE=mapledir ...
    (append
     (list
      (cond
       ((eq system-type 'gnu/linux)
	(concat "LD_LIBRARY_PATH=" bindir ":$LD_LIBRARY_PATH"))
       ((member system-type '(windows-nt cygwin ms-dos))
	(format "PATH=\"%s;%%PATH%%\"" bindir))
       ((eq system-type 'darwin)
	(concat "DYLD_LIBRARY_PATH=" bindir ":$DYLD_LIBRARY_PATH"))
       (t (error "Unexpected system-type '%s'" system-type)))
      (concat "MAPLE=" mapledir))
     (if maplev-use-new-language-features
	 (cons "MAPLE_NEW_LANGUAGE_FEATURES=1" process-environment)
       process-environment))))

(defun maplev-cmaple--get-pmaple-and-options ()
  "Return a list of strings consisting of the pmaple executable and its options."
  (let ((pmaple (slot-value maplev-config 'pmaple)))
    (unless pmaple
      (setq pmaple (maplev-cmaple-default-pmaple)))
    (cond
     ((not (file-exists-p pmaple))
      (error "The pmaple file, '%s', does not exist" pmaple))
     ((not (file-exists-p pmaple))
      (error "The pmaple file, '%s', is not executable" pmaple))
     (t
      (cons pmaple (maplev-get-option-with-include maplev-config :maple-options "-q"))))))

(defun maplev-cmaple--start-process ()
  "Start a cmaple process associated with the current buffer.
Return the process.  If such a process already exists, kill it and
restart it.  If variable `maplev-config' is assigned, use it, otherwise create
one by calling function `maplev-config'."
  (let* ((config (or maplev-config (maplev-config)))
	 (process-environment (maplev-cmaple--process-environment))
	 (pmaple-and-opts (maplev-cmaple--get-pmaple-and-options))
         (buffer (get-buffer-create (maplev--cmaple-buffer)))
         (process (get-buffer-process buffer))
         ;; Just testing this.  Is there an advantage to a PTY process?
	 (process-connection-type 'pty))
    (with-current-buffer buffer
      (message "Starting Maple...")
      (if process (delete-process process))
      (if maplev-startup-directory
          (cd (expand-file-name maplev-startup-directory)))
      (if maplev-load-path
       	(setenv "LD_LIBRARY_PATH" maplev-load-path))
      (set-process-filter
       (setq process (apply #'start-process
                            "Maple"
			    buffer
                            pmaple-and-opts))
       'maplev--cmaple-filter)
      (maplev-cmaple-setup config)
      (message "Maple started")
      process)))


(defun maplev-cmaple-send ()
  "Send input to Maple."
  (interactive)
  (let ((pmark (process-mark (maplev--cmaple-process)))
        (comint-input-sender #'maplev-cmaple--send-string))
    ;; Only _new_ input is checked for typos, see comint-send-input.
    ;; We might need something smarter for comint-get-old-input.
    ;; Why does comint-send-input use (line-end-position) instead of
    ;; (point-max)?  To be consistent maplev-mint-region does the same.
    (if (or (< (point) (marker-position pmark))
	    (zerop (maplev-mint-region pmark (line-end-position) 'syntax-only)))
        (comint-send-input))))

(defun maplev-cmaple--send-string (process maple-input &optional echo)
  "Send MAPLE-INPUT to the cmaple PROCESS.
If ECHO is non-nil, print MAPLE-INPUT to the output buffer."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (when echo
      (insert-before-markers maple-input ?\n)))
  (set-process-filter process #'maplev--cmaple-filter)
  (comint-simple-send process
		      ;; trim white-space at end of MAPLE and append a null character;
		      ;; the null character is the delimiter used by pmaple.
		      (concat (if (string-match "\\s-+$" maple-input)
				  (replace-match "" nil nil maple-input)
				maple-input)
			      (string ?\0))))

(defun maplev-cmaple-send-region (beg end)
  "Send the region from BEG to END to cmaple.
If called interactively use the marked region.
If called with a prefix the cmaple buffer is first cleared.
Use mint to syntax check the region before sending to cmaple."
  (interactive "r")
  (when (equal 0 (maplev-mint-region beg end 'syntax-only))
    (when current-prefix-arg
      (maplev-cmaple--clear-buffer))
    (maplev-cmaple--send-string (maplev--cmaple-process)
				(buffer-substring-no-properties beg end)
				'echo)))

(defun maplev-cmaple-send-line ()
  "Send the current line to cmaple."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (maplev-cmaple-send-region (point) (line-end-position))))

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
from the output buffer."
  ;; This may not work on a Windows box; there, the input is not echoed
  ;; to the output buffer.
  (interactive)
  (let ((proc (maplev--cmaple-process))) ; ensure Maple is started
    (with-current-buffer (maplev--cmaple-buffer)
      (save-restriction
        (narrow-to-region (point-max) (point-max))
	(let ((begin (+ 5 (point))))
	  (maplev-cmaple--send-string proc input)
	  (while (or (< (point) begin)
		     (progn
		       (goto-char (- (point-max) 5))
		       (not (looking-at "(\\*\\*) "))))
	    (sleep-for 0.01)))
        (let ((output (buffer-substring-no-properties
		       (point-min) (if (= (point) (point-min))
				       (point)
				     (1- (point))))))
          (if delete
              (delete-region (point-min) (point-max)))
          output)))))

(defun maplev-cmaple-interrupt ()
  "Interrupt Maple."
  (interactive)
  (let ((process (get-buffer-process (maplev--cmaple-buffer))))
    (if (null process)
	(error "The buffer has no process")
      (message "Interrupt process %s" (process-name process))
      (interrupt-process process))))

(defun maplev-cmaple-kill ()
  "Kill Maple."
  (interactive)
  (let ((process (get-buffer-process (maplev--cmaple-buffer))))
    (if (null process)
	(message "No maple process")
      (message "Kill process %s" (process-name process))
      (kill-process process))))

(defun maplev-cmaple--clear-buffer ()
  "Clear the contents of the cmaple buffer."
  (with-current-buffer (maplev--cmaple-buffer)
    (delete-region (point-min) (point-max))))
                 

(defun maplev-cmaple-pop-to-buffer ()
  "Pop up a buffer with command line Maple.  Start Maple, if necessary."
  (interactive)
  (maplev--cmaple-process)
  (pop-to-buffer (maplev--cmaple-buffer))
  (goto-char (point-max)))

(defalias 'cmaple 'maplev-cmaple-pop-to-buffer)

(defun maplev--cmaple-filter (process maple-output)
  "Send the string MAPLE-OUTPUT to the Maple buffer.
PROCESS is the Maple process."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert-before-markers maple-output)))

(defun maplev--cleanup-buffer ()
  "Remove over-striking and underlining from the current buffer."
  (goto-char (point-min))
  (while (re-search-forward "\e\\[[0-9;]+m" nil t) (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\r+" nil t) (replace-match "\n")))

(defun maplev-cmaple-newline ()
  "Insert a newline but do not send input to pmaple."
  (interactive)
  (insert ?\n maplev-cmaple-prompt))

;;}}}
;;{{{ mode map

(defvar maplev-cmaple-mode-map
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
    (define-key map [(shift return)]             'maplev-cmaple-newline)
    map)
  "Keymap used in Maple cmaple mode.")

;;}}}
;;{{{ mode definition

(defconst maplev-input-line-keyword
  `((,(concat "^" (regexp-quote maplev-cmaple-prompt)) . maplev-input-face))
  "Keyword for font locking input lines in `maplev-cmaple-mode'.")

(define-derived-mode maplev-cmaple-mode comint-mode
  "Major mode for interacting with cmaple.

This mode has the same commands as `comint-mode' plus some
additional commands for interacting with cmaple.

\\{maplev-cmaple-map}"
  
  (setq comint-prompt-regexp (concat "^\\(" (regexp-quote maplev-cmaple-prompt) "\\)+ *")
        comint-eol-on-send t  ; goto end of line before sending
        mode-name "Maple"
	comint-use-prompt-regexp t)

  ;; Mint support
  (make-local-variable 'maplev-mint--code-beginning)
  (make-local-variable 'maplev-mint--code-end)

  ;; font lock support
  ;; (set (make-local-variable 'font-lock-defaults)
  ;;      '(maplev-input-line-keyword))
  (set (make-local-variable 'comint-process-echoes) t)
  (font-lock-mode 1))

(defun maplev-cmaple-setup (config)
  "Set `major-mode' to `maplev-cmaple-mode'.
CONFIG is an object of type `maplev-config-class."
  (maplev-cmaple-mode)
  (setq maplev-config config))

  

;;}}}

(provide 'maplev-cmaple)

;;; maplev-cmaple.el ends here
