;;; maplev-help.el --- Maple help

;;; Commentary:
;; 

;;; Code:


(require 'comint)
(require 'maplev-re)
(require 'maplev-custom)
(require 'maplev-common)
(require 'maplev-history)
(require 'maplev-utils)

(declare-function maplev--cleanup-buffer "maplev-cmaple")
(declare-function maplev--cmaple-buffer "maplev-cmaple")
(declare-function maplev--cmaple-process "maplev-cmaple")
(declare-function maplev--proc-buffer "maplev-view")
(declare-function maplev-cmaple-direct "maplev-cmaple")

;;{{{ mode map

(defvar maplev-help-mode-map
  (let ((map (make-sparse-keymap)))
;;    (define-key map [(SPC)]                      'scroll-up)
    (define-key map (read-kbd-macro "SPC")       'scroll-up)
    (define-key map [(backspace)]                'scroll-down)
    (define-key map [?q]                         'quit-window)
    (define-key map [?s]                         'isearch-forward)
    (define-key map [?r]                         'maplev-history-redo-item)
    (define-key map [?p]                         'maplev-history-prev-item)
    (define-key map [?n]                         'maplev-history-next-item)
    (define-key map [?d]                         'maplev-history-delete-item)
    (define-key map [?P]                         'maplev-help-parent)
    (define-key map [?\?]                        'maplev-help-at-point)
    (define-key map [(control ?\?)]              'maplev-help-at-point)
    (define-key map [(meta ?\?)]                 'maplev-view-at-point)
    (define-key map [?f]                         'maplev-tear-off-window)
    (define-key map [(control c) (control s) ?h] 'maplev-switch-buffer-help)
    (define-key map [(control c) (control s) ?l] 'maplev-switch-buffer-proc)
    (define-key map [(control c) (control s) ?c] 'maplev-switch-buffer-cmaple)
    (define-key map [(control c) (control c)]    'maplev-help-reset-help)
    (define-key map [?h]                         'maplev-switch-buffer-help) ; short-cut
    (define-key map [?l]                         'maplev-switch-buffer-proc) ; short-cut
    (define-key map [?c]                         'maplev-switch-buffer-cmaple) ; short-cut
    (define-key map [(return)]                   'maplev-help-at-point)
    (define-key map [(meta return)]              'maplev-view-at-point)

    ;; Bind mouse buttons
    (define-key map [(mouse-2)]               'maplev-help-follow-mouse)
    (define-key map [(shift mouse-2)]         'maplev-help-follow-mouse)
    (define-key map [(control shift mouse-2)] 'maplev-help-follow-mouse)

    (define-key map [(meta mouse-2)]          'maplev-view-follow-mouse)
    (define-key map [(meta shift mouse-2)]    'maplev-view-follow-mouse)
    map)
  "Keymap used in `maplev-help-mode'.")

(defvar maplev-help-mode-menu nil)
(unless maplev-help-mode-menu
  (easy-menu-define
    maplev-help-mode-menu maplev-help-mode-map
    "Menu for Maple help and proc buffer."
    '("MapleV"
      ["Parent"         maplev-help-parent
       :included (eq major-mode 'maplev-help-mode)]
      ["Previous"       maplev-history-prev-item t]
      ["Next"           maplev-history-next-item t]
      ["Redraw"         maplev-history-redo-item t]
      ["Delete"         maplev-history-delete-item t]
      ["Goto help node" maplev-help-at-point t]
      ["Goto proc node" maplev-view-at-point t]
      ["Clear history"  maplev-history-clear t]
      ["Reset"          maplev-help-reset-help t]
      "---"
      ["Separate frame" maplev-tear-off-window
       :active (not (one-window-p t 'here))]
      "---"
      ("Decoration" :included (eq major-mode 'maplev-view-mode)
       ["reserved words"  (maplev-reset-font-lock 1) :style radio
	:selected (equal font-lock-maximum-decoration 1)]
       ["+ special words"  (maplev-reset-font-lock 2) :style radio
	:selected (equal font-lock-maximum-decoration 2)]
       ["+ builtin functions"  (maplev-reset-font-lock 3) :style radio
	:selected (or (equal font-lock-maximum-decoration 3)
		      (equal font-lock-maximum-decoration t))]))))

;;}}}
;;{{{ mode definition

(define-derived-mode maplev-help-mode fundamental-mode
  "Major mode for displaying Maple help pages.

\\{maplev-help-mode-map}"

  (set (make-local-variable 'maplev--process-item) #'maplev--help-process)

  (make-local-variable 'maplev-history--stack) ; set up the stack
  (maplev-history-clear)

  ;; for maplev--activate-hyperlinks
  (set (make-local-variable 'parse-sexp-lookup-properties) t)

  (maplev-help-fontify-node)
  (setq buffer-read-only t))

(defun maplev-help-setup (&optional config)
  "Unless already assigned, set `major-mode' to `maplev-help-mode'.
The optional CONFIG argument is an object of type
`maplev-config-class'.  Its default is the variable
`maplev-config' or `maple-config-default', in that order."
  (unless (eq major-mode 'maplev-help-mode)
    (maplev-help-mode))
  (setq maplev-config (or config maplev-config maplev-config-default)
	mode-name (format "Maple-Help: %s" (slot-value maplev-config 'maple))))

;;}}}
;;{{{ mode functions

(defun maplev--help-buffer ()
  "Return the name of the Maple help buffer."
  (concat "Maple help" (and maplev-config
			    (format " (%s)" (slot-value maplev-config 'maple)))))

(defun maplev-help-follow-mouse (click)
  "Display the Maple help page of the topic at the mouse CLICK."
  (interactive "e")
  (set-buffer (window-buffer (event-window click)))
  (goto-char (event-point click))
  (let ((topic (replace-regexp-in-string "\n" "" (maplev--ident-around-point) 'inplace))
        (pkg (maplev-help--get-package)))
    (if pkg
        ;; This frequently works when the help index does not have a
        ;; link to the particular help page; I understand that that is
        ;; a deficiency only with the tty help for smaple.
        (setq topic (format "%s,%s" pkg topic)))
    (maplev-help-show-topic topic)))

(defun maplev-help--get-package ()
  "Check whether the help page is a package overview.
If so, return the name of the package, otherwise return nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (and (looking-at "\\(?:Details\\|Overview\\) of the \\(\\w+\\) [Pp]ackage")
           (match-string-no-properties 1)))))

(defun maplev-help-at-point (topic)
  "Display Maple help for TOPIC \(a string\).
Interactively, default is word point is on."
  (interactive (list (maplev-ident-around-point-interactive
                      "Maple help topic" "help" t)))
  (maplev-help-show-topic topic))

(defun maplev-help-show-topic (topic &optional hide)
  "Display Maple help for TOPIC \(a string\).
Push TOPIC onto the local stack, unless it is already on the top.
If HIDE is non-nil, do not bring buffer to front."
  (unless
      (and maplev-help-use-standard-flag
	   (maplev-help-standard-help topic))
      (let ((config maplev-config))
	(with-current-buffer (get-buffer-create (maplev--help-buffer))
	  (maplev-help-setup config)
	  ;; Push TOPIC onto history stack
	  ;; The magic is done by 'maplev--process-item, which
	  ;; is buffer-local and assigned maplev--help-process.
	  (maplev-history--stack-process topic hide)))))

(defun maplev--help-process (topic)
  "Display Maple help for TOPIC in `maplev--help-buffer'."
  (let ((process (maplev--cmaple-process)))
    ;; (maplev-cmaple--lock-access)
    (set-process-filter process 'maplev--help-filter)
    (set-buffer (maplev--help-buffer))
    (setq mode-line-buffer-identification (format "%-12s" topic))
    (let (buffer-read-only)
      (delete-region (point-min) (point-max)))
    (comint-simple-send process (concat
				 "interface('screenheight=infinity'):"
				 ;; "kernelopts('printbytes'=false):"
				 "help(\"" topic "\");"
				 (string ?\0)))))

(defun maplev--help-filter (process string)
  "Pipe the output of a help command into `maplev--help-buffer'.
PROCESS calls this filter.  STRING is the output."
  (with-current-buffer (maplev--help-buffer)
    (save-excursion
      (let (buffer-read-only)
        (save-restriction
          (goto-char (point-max))
          (narrow-to-region (point) (point))
          (insert string)
          (maplev--cleanup-buffer))
	(goto-char (point-max))
	(save-excursion
	  (beginning-of-line)
	  (when (looking-at "(\\*\\*) ")
	    (delete-region (point) (point-max))
	    (maplev-help--cleanup-buffer)))))))

(defun maplev-help--cleanup-buffer ()
  "Cleanup Maple help pages."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "(\\*\\*) ")
      (delete-region (point) (match-end 0))))
  (maplev-help-fontify-node)
  (set-buffer-modified-p nil))

(defun maplev-switch-buffer-help ()
  "Switch to help buffer, if it exists."
  (interactive)
  (maplev-switch-buffer (maplev--help-buffer)))

(defun maplev-switch-buffer-proc ()
  "Switch to proc buffer, if it exists."
  (interactive)
  (maplev-switch-buffer (maplev--proc-buffer)))

(defun maplev-switch-buffer-cmaple ()
  "Switch to cmaple buffer, if it exists."
  (interactive)
  (maplev-switch-buffer (maplev--cmaple-buffer)))

(defun maplev-switch-buffer (buffer)
  "Switch to BUFFER, if it exists."
  (let ((buf (get-buffer buffer)))
    (if buf
        (switch-to-buffer buf)
      (message "No buffer \"%s\"." buffer))))

(defun maplev-help-to-source (code-only)
  "Convert a help page to a Maple source file."
  (interactive "P")
  (let (buffer-read-only)
    (goto-char (point-min))
    (while (not (eobp))
      (if (looking-at "> ")
          (progn
            (delete-char 2)
            (forward-line))
        (if (not code-only)
            (progn
              (insert "# ")
              (forward-line))
          (delete-region (point) (line-end-position))
          (delete-char 1))))))

(defun maplev-help-reset-help ()
  "Reset the settings that affect the display of help pages."
  (interactive))
;;  (maplev-cmaple-direct " 'delete))

(defun maplev-help-standard-help (topic)
  "Display help for TOPIC in the Standard help browser.
If successful, return t, otherwise return nil."
  (condition-case nil
      (let ((tcp-proc (open-network-stream "tcp-proc" nil "localhost" maplev-help-port))
	    (request (format "help(%s)" topic)))
	(process-send-string tcp-proc request)
	(delete-process tcp-proc)
	(message "Help sent to Maple GUI")
	t)
    (error
       (message "cannot connect to Maple help server")
       (beep))))

(defun maplev-help-toggle-standard-help (&optional arg)
  "Toggle whether to use the standard Maple help browser.
With prefix argument ARG, use it if ARG is positive, otherwise
use the tty help browser, in an Emacs buffer."
  (interactive "P")
  (setq maplev-help-use-standard-flag
	(if (null arg)
	    (not maplev-help-use-standard-flag)
	  (> (prefix-numeric-value arg) 0)))
  (message "Standard help %s"
	   (if maplev-help-use-standard-flag
	       "enabled" "disabled")))

(defun maplev-launch-standard-gui-with-server ()
  "Launch the Maple Standard GUI, opening a worksheet that starts the MapleServer.
The MapleServer displays help pages and worksheets upon request."
  (interactive)
  (let* ((mw (expand-file-name "~/maple/toolbox/MapleServer/data/MapleServer.mw"))
	 (cmd (format "maple -x \"%s\" &" mw)))
    (if (file-exists-p mw)
	(shell-command cmd))))

;;}}}
;;{{{ history mechanism

(defun maplev-help-parent ()
  "Display the parent node of the current help page.
The parent node is extracted from the context of the help page, not
from the parent defined in the Maple help system."
  (interactive)
  (goto-char (point-min))
  (if (looking-at "\\(Function: ?\\)?\\([a-zA-Z0-9]*\\)\\[")
      (maplev-help-show-topic (match-string 2))
    (maplev-help-show-topic "index")))

;;}}}
;;{{{ fontify

;;{{{ (*) fonts

(defcustom maplev-help-function-face 'font-lock-function-name-face
  "Face name for functions in title lines of Maple help pages."
  :type 'face
  :group 'maplev-faces
  :group 'maplev-help)

(defvar maplev-help-title-face   'maplev-help-title-face
  "*Face name for subtitles in title lines of Maple help pages.")

(defvar maplev-help-section-face 'maplev-help-section-face
  "*Face name for section titles in Maple help pages.")

(defvar maplev-help-subsection-face 'maplev-help-section-face
  "*Face name for section titles in Maple help pages.")

(defvar maplev-input-face  'maplev-input-face
  "*Face name for Maple input in help pages and Maple buffer.")

(defface maplev-help-title-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))  (:foreground "DimGray"   :bold t))
    (((class color)     (background light)) (:foreground "Black"     :bold t))
    (((class color)     (background dark))  (:foreground "Green"     :bold t))
    (t (:bold t)))
  "Font lock mode face used to highlight subtitles in Maple help pages.
The title is the phrase following the function name."
  :group 'maplev-faces
  :group 'maplev-help)

(defface maplev-help-section-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))  (:foreground "DimGray"   :bold t))
    (((class color)     (background light)) (:foreground "Red"       :bold t))
    (((class color)     (background dark))  (:foreground "Red"       :bold t))
    (t (:bold t)))
  "Font lock mode face used to highlight section titles in Maple help pages."
  :group 'maplev-faces
  :group 'maplev-help)

(defface maplev-help-subsection-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))  (:foreground "DimGray"   :bold t))
    (((class color)     (background light)) (:foreground "orange"    :bold t))
    (((class color)     (background dark))  (:foreground "orange"    :bold t))
    (t (:bold t)))
  "Font lock mode face used to highlight section titles in Maple help pages."
  :group 'maplev-faces
  :group 'maplev-help)

(defface maplev-input-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))  (:foreground "DimGray"   :bold t))
    (((class color)     (background light)) (:foreground "dark green"))
    (((class color)     (background dark))  (:foreground "green"))
    (t (:bold t)))
  "Font lock mode face used to highlight Maple input."
  :group 'maplev-faces
  :group 'maplev-help)

;;}}}
;;{{{ (*) regular expressions

(defconst maplev--help-section-re
  (concat "^\\(Calling Sequences?"
          "\\|Parameters"
          "\\|Description"
          "\\|Examples"
          "\\|See Also"
	  "\\| ?Pages That Link to This Page"
          "\\|References"
          "\\|Returns"
          "\\|Notes"
          "\\|Options"
          "\\|Algorithm"
	  "\\|Compatibility"
	  "\\|Thread Safety"
          "\\|\\(?:List of \\([][a-zA-Z_]+ \\)?\\(Package\\|Subpackage\\|Module\\) Commands\\)"
          "\\):?")
  "Regular expression for sections in a Maple help page.")

(defconst maplev--help-subsection-re
  (concat "^\\([A-Z][a-z-0-9-]+ ?\\([A-Za-z0-9-][a-z]* ?\\)?"
          "\\([A-Za-z][a-z-]*\\)?:?[ \t]*$"
          "\\)")
  "Regular expression for subsections in a Maple help page.")

(defconst maplev--help-definition-re
  "([ \t\n]*\\(Definition/[^) \t\n]+\\)[ \t\n]*)"
  "Regular expression for dictionary hyperlinks.")

;;}}}
;;{{{ (*) functions

(defun maplev-help-fontify-node ()
  "Fontify a Maple help page buffer.  Does not use font-lock mode."
  (save-excursion
    (let (buffer-read-only
          (case-fold-search t))
      (font-lock-mode -1) ; turn-off font-lock.

      ;; Highlight the title.
      ;; The tricky part is handling multiple titles.

      (goto-char (point-min))
      (if (looking-at "No exact matches found, please try one of the following (\\?#number):")
	  ;; create links for all the numbered help pages
	  (while (re-search-forward "^\\( *\\)\\([0-9]+\\.\\) \\(.*\\)$" nil 'move)
	    (let ((b (match-beginning 2))
		  (e (match-end 2)))
	    (replace-match "\\1#\\2 \\3")
	    (maplev--activate-hyperlink b e)))
	;; Move to the end of the title area.  Stop at first section or bullet.
	(if (re-search-forward (concat maplev--help-section-re "\\|^- ")
			       nil 'move)
	    ;; Move backward to top of buffer, checking each line.
	    (while (= 0 (forward-line -1))
	      (if (looking-at "\\(Function:\\)?\\([^-\n]*\\)[ \t]+-[ \t]+\\(.*\\)$") ; regexp for function name(sort of)
		  (progn (and (match-beginning 1)
			      (put-text-property (match-beginning 1) (match-end 1)
						 'face 'maplev-help-section-face))
			 (and (match-beginning 3)
			      (put-text-property (match-beginning 3) (match-end 3)
						 'face 'maplev-help-title-face))
			 (and (match-beginning 2)
			      (maplev--activate-hyperlinks (match-beginning 2) (match-end 2))))
		(put-text-property (point) (progn (end-of-line) (point))
				   'face 'maplev-help-title-face)))
	  (goto-char (point-min))
	  (end-of-line)
	  (put-text-property (point-min) (point)
			     'face 'maplev-help-title-face))

	;; Highlight subsection titles
	(goto-char (point-min))
	(while (re-search-forward maplev--help-subsection-re nil t)
	  (put-text-property (match-beginning 0) (match-end 0)
			     'face 'maplev-help-subsection-face))


	;; Highlight functions in a package. This usually works.  It
	;; searches for `- The functions [arbitrary text] are:' and
	;; highlights everything from the colon to the next line that
	;; starts with a character that is not whitespace.
	(goto-char (point-min))
	(when (re-search-forward
	       "^- The\\( available\\)? \\(functions\\|routines\\)[^\n]* are\\( the following\\)?: *$"
	       nil 'move)
	  (maplev--activate-hyperlinks
	   (point) (progn (re-search-forward "^[^ \t\n]" nil 'move)
			  (line-end-position -1))))

	;; Highlight Maple input
	(goto-char (point-min))
	(while (re-search-forward "^> .*$" nil t)
	  (put-text-property (match-beginning 0) (match-end 0)
			     'face 'maplev-input-face))

	;; Highligt Maple comments
	(goto-char (point-min))
	(while (re-search-forward "^# .*$" nil t)
	  (put-text-property (match-beginning 0) (match-end 0)
			     'face 'font-lock-comment-face))
	


	;; Activate hyperlinks following "See Also" and "Pages That Link to This Page".
	(goto-char (point-max))
	(let ((end (point))
	      tmp)
	  (when (re-search-backward "^ ?Pages That Link to This Page" nil 'move)
	    (setq tmp (point))
	    (maplev--activate-hyperlinks (match-end 0) end)
	    (setq end tmp)
	    (goto-char end))
	  (goto-char end)
	  (when (re-search-backward "^See Also:?" nil 'move)
	     (maplev--activate-hyperlinks (match-end 0) end)))


	;; Highlight section titles
	(goto-char (point-min))
	(while (re-search-forward maplev--help-section-re nil t)
	  (put-text-property (match-beginning 0) (match-end 0)
			     'face 'maplev-help-section-face))


	;; Activate hyperlinks in text.  This is overly aggressive.
	(goto-char (point-min))
	(re-search-forward "^Description" nil t)
	(while (re-search-forward "(\\([][a-zA-Z,]+\\))" nil 'move)
	  (save-excursion
	    (beginning-of-line)
	    (unless (looking-at "> ")
	      (maplev--activate-hyperlink (match-beginning 1) (match-end 1)))))
	

	;; Activate hyperlinks following "Multiple matches:".
	(goto-char (point-min))
	(and (re-search-forward "^Multiple matches found:" nil 'move)
	     (maplev--activate-hyperlinks (match-end 0) (point-max)))

	;; Active dictionary hyperlinks
	(goto-char (point-min))
	(while (re-search-forward maplev--help-definition-re nil 'move)
	  (let ((beg (match-beginning 1))
		(end (match-end 1)))
	    ;;(put-text-property beg end 'mouse-face 'highlight)
	    ;;(put-text-property beg end 'face maplev-help-function-face))))))
	    (maplev--activate-hyperlink beg end)))))))

(defun maplev--activate-hyperlinks (beg end)
  "Font lock and activate Maple keywords in the region from BEG to END."
  (goto-char beg)
  (while (re-search-forward
	  " *\\([^,/\n]+\\(/\n?[^,/\n]+\\)*\\)\\(,\\|$\\)"
          end 'move)
    (let ((beg (match-beginning 1))
          (end (match-end 1)))
      ;; Treat everything between beg and end as word constituents.
      ;; In particular, ignore the syntactic meaning of, e.g., `[',
      ;; `]', and `,'. Thus we can use current-word to pick up
      ;; these Maple keywords.
      (maplev--activate-hyperlink beg end))))

(defun maplev--activate-hyperlink (beg end)
  "Font lock and activate text in region from BEG to END."
  (put-text-property beg end 'syntax-table '(2 . nil))
  (put-text-property beg end 'mouse-face 'highlight)
  (put-text-property beg end 'face maplev-help-function-face))


;;}}}

;;}}}

(provide 'maplev-help)

;;; maplev-help.el ends here
