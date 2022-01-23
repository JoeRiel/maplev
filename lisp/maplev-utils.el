;;; maplev-utils.el --- Utility functions for maplev



;;; Commentary:
;; 

;;; Code:

(require 'maplev-re)
(require 'compile)
(require 'subr-x)

(eval-when-compile
  (defvar mouse-selection-click-count)
  (defvar compilation-error-regexp-alist)
  (autoload 'mouse-selection-click-count "mouse"))

(declare-function maplev-ident-around-point-interactive "maplev-common")
(declare-function maplev-cmaple-direct "maplev-cmaple")

(defun maplev--string-to-name (name)
  "Convert NAME to a valid Maple name.  Add back-quotes if needed."
  ;; Do we need something more general to match a string that might
  ;; require backquotes?
  (unless (string-match maplev--simple-name-re name)
    (if (not (string= "`" (substring name 0 1)))
        (setq name (concat "`" name)))
    (if (not (string= "`" (substring name -1)))
        (setq name (concat name "`"))))
  name)

(when (fboundp 'x-get-cut-buffer)
  ;; Xmaple doesn't support selections
  (defun maplev-insert-cut-buffer (&optional arg)
    "Insert the value of the X server cut-buffer 0.
Add text string to the kill ring.  Interpret ARG as \\[yank] does."
    (interactive "*P")
    (kill-new (x-get-cut-buffer 0))
    (setq this-command 'yank)
    (yank arg))
  
  ;; borrowed from mouse-yank-at-click
  (defun maplev-mouse-yank-cut-buffer (click arg)
    "Insert the value of the X server cut-buffer 0 at the position of CLICK.
Move point to one end of the text thus inserted (normally the
end), and set mark at the beginning.  Interpret ARG as \\[yank]
does.  If `mouse-yank-at-point' is non-nil, insert at point
regardless of where you click."
    (interactive "e\nP")
    (kill-new (x-get-cut-buffer 0))
    ;; Give temporary modes such as isearch a chance to turn off.
    (run-hooks 'mouse-leave-buffer-hook)
    (or mouse-yank-at-point (mouse-set-point click))
    (setq this-command 'yank)
    (setq mouse-selection-click-count 0)
    (yank arg)))

(defun maplev-add-maple-to-compilation ()
  "Modify variables to fontify the compilation buffer.
Add the symbol \`maple to `compilation-error-regexp-alist' and
the `maplev--compile-error-re' regexp to
`compilation-error-regexp-alist-alist'.  This fontifies the
compilation buffer when using the `compile' command to build
Maple libraries.  Requires customizing `compile-command'."
  (interactive)
  (unless (member 'maple compilation-error-regexp-alist)
    (add-to-list 'compilation-error-regexp-alist 'maple)
    (add-to-list 'compilation-error-regexp-alist-alist `(maple ,maplev--compile-error-re 2 1 nil))))

(defun maplev-split-shell-option-string (opt)
  "Convert OPT, a string of command line options, into a list of options.
Close up space between option and argument.
For example, \"-a -w 100\" becomes \(\"-a\" \"-w100\"\)."
  (let ((opt (string-trim opt)))
    (if (and
	 (> (length opt) 0)
	 (/= (aref opt 0) ?-))
	(error "Option string does not have leading hypen: %s" opt))
    (let ((opts (split-string (string-trim opt) "\\(^\\| +\\)-" t)))
      (mapcar (lambda (s)
		(concat "-" (if (string-match "^.\\( +\\)[^ ]" s)
				(replace-match "" t t s 1)
			      s)))
	      opts))))


(defun maplev-edit-source (proc)
  "Edit the source for the file PROC.
The default is the procedure name at point.
For this to work, the Maple global variable `debugger/editor`
must be appropriately assigned.  See the Maple help page for showstat."
  (interactive (list (maplev-ident-around-point-interactive
		      "Maple procedure")))
  (let ((cmd (format "editsource(%s):" proc)))
    (maplev-cmaple-direct cmd 'delete)))


(provide 'maplev-utils)

;;; maplev-utils.el ends here
