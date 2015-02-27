;;; maplev-custom.el --- Customizable parameters for maplev

;;; Commentary:
;; 

;;; Code:
;;

(require 'align)

;;{{{ Group definitions

(defgroup maplev nil
  "Major mode for editing Maple source in Emacs"
  :group 'languages)

(defgroup maplev-important nil
  "STUFF THAT MUST BE CONFIGURED."
  :group 'maplev)

(defgroup maplev-declarations nil
  "Customizations for declaring variables."
  :group 'maplev)

(defgroup maplev-faces nil
  "Faces for highlighting text in MapleV mode."
  :group 'maplev)

(defgroup maplev-executables nil
  "Maple and Mint location and configuration."
  :group 'maplev)

(defgroup maplev-templates nil
  "Procedure template and other shortcuts."
  :group 'maplev)

(defgroup maplev-misc nil
  "Miscellaneous options."
  :group 'maplev)

(defgroup maplev-align nil
  "Alignment variables."
  :group 'maplev)

(defgroup maplev-buffer nil
  "Maple buffer stuff \(mostly names\)."
  :group 'maplev)

(defgroup maplev-help nil
  "Maple help pages."
  :group 'maplev)

(defgroup maplev-mint nil
  "Mint setup."
  :group 'maplev
  :group 'maplev-executables)


;;}}}
;;{{{ Configurable options

;;{{{   executables

(defun maplev-windows-executables (major-release &optional num-exe-flag ini-file)
  "Return a list of the Maple executables for Windows.
MAJOR-RELEASE is the Maple major release number; if NUM-EXE-FLAG
is non-nil, MAJOR-RELEASE is appended to the cmaple
name.  Optional INI-FILE specifies an initialization file.  A list
of three elements is returned \[cmaple, ini-file, mint\]."
  (let ((num (if num-exe-flag major-release "")))
  (list (format "c:/Program Files/Maple Release %s/bin.wnt/cmaple%s.exe" major-release num)
        ini-file
        (format "c:/Program Files/Maple Release %s/bin.wnt/mint%s.exe" major-release num))))

(defcustom maplev-executable-alist
  (if (string-match "windows-nt\\|ms-dos" (symbol-name system-type))
      `(("16" . ,(maplev-windows-executables "16" t))
	("15" . ,(maplev-windows-executables "15" t))
        ("14" . ,(maplev-windows-executables "14" t))
        ("13" . ,(maplev-windows-executables "13" t))
        ("12" . ,(maplev-windows-executables "12" t))
        ("11" . ,(maplev-windows-executables "11" t))
        ("10" . ,(maplev-windows-executables "10" t))
        ("9"  . ,(maplev-windows-executables "9"  t))
        ("8"  . ,(maplev-windows-executables "8"  t))
        ("7"  . ,(maplev-windows-executables "7"  t))
        ("6"  . ,(maplev-windows-executables "6"  t))
        ("5"  . ,(maplev-windows-executables "5"  t))
        ("4"  . ,(maplev-windows-executables "4"  t))
        ("3"  . ,(maplev-windows-executables "3"  t)))
    '(
      ("16"  . ("maple" nil "mint"))
      ("15"  . ("maple" nil "mint"))
      ("14"  . ("maple" nil "mint"))
      ("13"  . ("maple" nil "mint"))
      ("12"  . ("maple" nil "mint"))
      ("11"  . ("maple" nil "mint"))
      ("10"  . ("maple" nil "mint"))
      ("9"   . ("maple" nil "mint"))
      ("8"   . ("maple" nil "mint"))
      ("7"   . ("maple" nil "mint"))
      ("6"   . ("maple" nil "mint"))
      ("5.1" . ("maple" nil "mint"))
      ("5"   . ("maple" nil "mint"))
      ("4"   . ("maple" nil "mint"))))
  "Assoc list specifying the available executables.
Each item has the form \(RELEASE MAPLE MAPLE-INIFILE MINT\)
where RELEASE is the Maple release corresponding to the
executables MAPLE and MINT.  MAPLE must be the command line
\(non-GUI\) version of Maple.  MAPLE-INIFILE is the maple
initialization file for running Maple under Emacs;
if nil the default initialization file is used."
  :type '(repeat (list (string :tag "Maple Release")
                       (file   :tag "Maple Executable")
                       (choice :tag "Maple Initialization File"
                               file (const :tag "none" nil))
                       (file   :tag "Mint Executable ")))
  :group 'maplev-executables
  :group 'maplev-important)

;; this isn't quite right, it doesn't permit assigning
;; a new release.

(defcustom maplev-default-release "15"
  "Release of Maple used as the default executable.
It must be a key in `maplev-executable-alist'."
  :type `(choice ,@(mapcar (lambda (item)
                             (list 'const (car item)))
                           maplev-executable-alist))
  :group 'maplev-executables
  :group 'maplev-important)

(defconst maplev-interface-kernelopts-settings
  (concat "interface('prettyprint=1,verboseproc=2,errorbreak=0,warnlevel=2,errorcursor=false,screenheight=infinity'):\n"
          "kernelopts('printbytes=false'):\n" )
  "Maple commands that assign the default interface and kernelopts settings." )

(defcustom maplev-default-init-string
  (concat
   "maplev_print := `if`(assigned(maplev['PrintProc']),maplev:-PrintProc,print):\n"
   maplev-interface-kernelopts-settings)
  "Default Maple commands used to initialize a Maple process.
Use `maplev-init-string-alist' to customize initialization commands
for particular releases.")

(defcustom maplev-init-string-alist
  (let ((init
         (concat
          "if not assigned(maplev_print) then maplev_print := proc(n)print(`if`(type(evaln(n),'procedure'),eval,readlib)(n))end:fi:\n"
          maplev-interface-kernelopts-settings)))
    `(("5.1" . ,init)
      ("5"   . ,init)))
  "Assoc list of Maple commands initializing a maple session.
Each item has the form \(RELEASE COMMANDS\) where RELEASE is the
Maple release.  COMMANDS must be a string of Maple commands.
Overrides `maplev-default-init-string'."
  :type '(repeat (cons (string :tag "Maple Release")
                       (string :tag "Maple Commands")))
  :group 'maplev-executables
  :group 'maplev-important)

(defcustom maplev-mint-info-level 3
  "Integer controlling amount of information that Mint outputs."
  :type '(choice (const :tag "no info" 0)
                 (const :tag "severe errors" 1)
                 (const :tag "+ serious errors" 2)
                 (const :tag "+ warnings" 3)
                 (const :tag "full report" 4))
  :group 'maplev-mint)

(defcustom maplev-mint-error-level 1
  "Integer controlling Mint error checking in Maple input."
  :type '(choice (const :tag "no info" 0)
                 (const :tag "severe errors" 1)
                 (const :tag "+ serious errors" 2)
                 (const :tag "+ warnings" 3)
                 (const :tag "full report" 4))
  :group 'maplev-mint)

(defcustom maplev-mint-start-options (list "-q" "-w 200")
  "List of mint command line options.
Do not include the info level or the include path,
they are handled by `maplev-mint-info-level' and `maplev-include-path'.
The line-width option (-w) is used to ensure that a reference to 
an included file appears on a single line."
  :type 'list
  ;;   :type '(repeat (choice (const :tag "no logo" " -q")
  ;;                       (const :tag "suppress startup" " -s")
  ;;                       (const :tag "syntax only" " -S")
  ;;                       (const :tag "cross reference" " -x")
  ;;                       (list :tag "library" (const " -b") directory)
  ;;                       (list :tag "append database" (const " -a ") file)
  ;;                       (list :tag "use database" (const " -d ") file)
  ;;                       (list :tag "toggle error" (const " -t ") (string :tag "error number"))))

  :group 'maplev-mint)

(defcustom maplev-include-path nil
  "List of directories to search for files to include.
Each element is a string (directory name) or nil.
The directories are passed to maple and to mint
via the \"-I\" option; they are searched for files
specified in Maple preprocessor $include directives."
  :type '(choice (const nil) (repeat string))
  :group 'maplev-executables
  :group 'maplev-mint)

(make-variable-buffer-local 'maplev-include-path)

;;}}}
;;{{{   comments

(defcustom maplev-comment-column 40
  "Column for inline comments.
Use \\[indent-for-comment] to insert or align an inline comment."
  :type 'integer
  :group 'maplev-comments)

(defcustom maplev-comment-start "#"
  "String to insert to start a Maple inline comment."
  :type 'string
  :group 'maplev-comments)

;; not used by GNU emacs 21
(defcustom maplev-block-comment-start "# "
  "String to insert to start a Maple standalone comment."
  :type 'string
  :group 'maplev-comments)

(defcustom maplev-auto-fill-comment-flag t
  "Non-nil means initially enable `auto-fill-mode' in a Maple buffer."
  :type 'boolean
  :group 'maplev-comments)

;;}}}
;;{{{   declarations

(defcustom maplev-var-declaration-symbol " :: "
  "Separator inserted between declared variable and type."
  :type 'string
  :group 'maplev-declarations)

(defcustom maplev-alphabetize-declarations-p nil
  "If non-nil, variable declarations are alphabetized.
Only works if `maplev-add-declaration-function' is assigned
either `maplev-add-declaration-leading-comma' or
`maplev-add-declaration-trailing-comma'."
  :type 'boolean
  :group 'maplev-declarations)

(defcustom maplev-add-declaration-function 'maplev-add-declaration-trailing-comma
  "Selects the function that adds variables to a declaration."
  :type '(radio
          (function-item :doc "declarations on one line" maplev-add-declaration-one-line)
          (function-item :doc "declarations on separate lines, with leading comma" maplev-add-declaration-leading-comma)
          (function-item :doc "declarations on separate lines, with trailing comma" maplev-add-declaration-trailing-comma))
  :group 'maplev-declarations)




;;}}}
;;{{{   indentation

(defcustom maplev-indent-level 4
  "Indentation of Maple statements with respect to containing block."
  :type 'integer
  :group 'maplev-indentation)

(defcustom maplev-indent-declaration-level 0
  "Indentation of Maple declarations \(local, global, option, description\)."
  :type 'integer
  :group 'maplev-indentation)

(defcustom maplev-dont-indent-re "[#$]"
  "Lines starting with this regular expression will not be auto-indented."
  :type '(choice string (const :tag "default" nil))
  :group 'maplev-indentation)

(defcustom maplev-auto-break-strings-flag t
  "Non-nil means strings in code will be automatically broken when they pass the `current-fill-column'."
  :type 'boolean
  :group 'maplev-indentation)

(defcustom maplev-tab-width 4
  "Tab width in maplev buffers."
  :type 'integer
  :group 'maplev-indentation)

;;}}}
;;{{{   templates

(defcustom maplev-copyright-owner "John Q. Public"
  "Copyright owner inserted in the copyright string by `maplev--template-proc-module'."
  :type 'string
  :group 'maplev-templates
  :group 'maplev-important)

(defcustom maplev-comment-end-flag t
  "Non-nil means add a template's name as a comment following the end.
See `maplev--template-proc-module'."
  :type 'boolean
  :group 'maplev-templates)

;;; The reason for making this [the following] customizable is to
;;; support mapledoc, a LaTeX package.  To hide the name of the
;;; template in the the typeset output, I use the string " #% ".  To
;;; display it I might use " #\# ", which also prints the hash.

(defcustom maplev-template-end-comment " # "
  "String prepended to the name of a template, following the \"end\".
See `maplev-comment-end-flag'."
  :type 'string
  :group 'maplev-templates)

(defcustom maplev-insert-copyright-flag t
  "Non-nil means insert `maplev-copyright-owner' in a template.
See `maplev-template'."
  :type 'boolean
  :group 'maplev-templates)

(defcustom maplev-description-quote-char ?\`
  "Quote character for the description statement.
Maple uses a backquote; however, in R5 it makes more sense to use a
double quote.  Procbody, alas, does not handle a double quote."
  :type 'character
  :group 'maplev-templates)

(defcustom maplev-variable-spacing 0
  "Spaces to insert after a comma in declarations and argument lists."
  :type 'integer
  :group 'maplev-templates)

(defcustom maplev-assignment-operator " := "
  "Maple assignment operator.  Used by `maplev-insert-assignment-operator'."
  :type 'string
  :group 'maplev-templates)

;;}}}
;;{{{   completion

(defcustom maplev-completion-longdelim-p nil
  "If non-nil use the long delimiter when completing a Maple control structure.
For example, if non-nil, a `do' loop is completed with `end do',
otherwise it is completed with `od'.  If the maple release is less than 6
than the long delimiter is never used."
  :type 'boolean
  :group 'maplev-completions)

;;}}}
;;{{{   miscellaneous

;; Leading commas

(defcustom maplev-leading-comma-flag t
  "Non-nil means the user prefers leading commas when continuing lines.
Currently this only determines whether advice for `fixup-whitespace'
is activated when `maplev-mode' is executed."
  :type 'boolean
  :group 'maplev-misc)

;; Abbrev mode

(defcustom maplev-initial-abbrev-mode-flag nil
  "Non-nil means initially enable function `abbrev-mode' in a Maple buffer."
  :type 'boolean
  :group 'maplev-misc)

(defcustom maplev-expand-abbrevs-in-comments-and-strings-flag nil
  "Non-nil means expand Maple abbreviations in comments and strings.
Nil means do not expand in either."
  :type 'boolean
  :group 'maplev-misc
  :group 'maplev-comments)

(defcustom maplev-include-file-other-window-flag t
  "Non-nil means open an include file in the other window.
See `maplev-find-include-file'."
  :type 'boolean
  :group 'maplev-misc)

;; Configuration

(defcustom maplev-buttonize-includes-flag t
  "Non-nil means use function `button-lock-mode' to hyperlink include statements."
  :type 'boolean
  :group 'maplev-misc)

(defcustom maplev-load-config-file-flag t
  "Non-nil means load a configuration file when starting `maplev-mode'.
The configuration file is named .maplev and is searched for in
the current directory and its ancestors.  The file is loaded as
an elisp file.  No error occurs if the file does not exist."
  :type 'boolean
  :group 'maplev-misc)

;; Saving

(defcustom maplev-clean-buffer-before-saving-flag t
  "Non-nil means run `maplev-remove-trailing-spaces' before saving."
  :type 'boolean
  :group 'maplev-misc)

;;}}}
;;{{{   align rules

;; Define the maplev alignment rules.
;; Align the assignment operator (`:='), equals signs,
;; columns (`|'), commas, double colons (`::'), and comments.
;; Columns and commas are aligned only if the
;; the prefix argument is active (i.e. C-u M-x align).
;; The comment rule is the last rule so that comments are properly aligned.

(eval-and-compile
  (when (featurep 'align)
    (defcustom maplev-align-rules-list
      '((maple-assignment-rule
         (regexp   . "\\s-*\\w+\\(\\s-*:\\)=\\(\\s-*\\)")
         (group    . (1 2))
         (justify  . t)
         (tab-stop . nil))
        (maple-equals-rule
         (regexp   . "\\s-*\\w+\\(\\s-*\\)=\\(\\s-*\\)")
         (group    . (1 2))
         (repeat   . t)
         (tab-stop . nil))
        (maple-type-rule
         (regexp   . "\\s-*\\w+\\(\\s-*\\)::\\(\\s-*\\)")
         (group    . (1 2))
         (repeat   . t)
         (tab-stop . nil))
        (maple-column-delimiter
         (regexp . "\\(\\s-*\\)\|\\(\\s-*\\)")
         (group  . (1 2))
         (repeat . t)
         (run-if lambda nil current-prefix-arg))
        (maple-comma-delimiter
         (regexp . ",\\(\\s-*\\)\\S-")
         (repeat . t)
         (run-if lambda nil current-prefix-arg))
        (maple-comment
         (regexp . "\\(\\s-+\\)\\s<")
         (column . comment-column)))
      "A list describing the maplev alignment rules.
See the documentation for `align-rules-list' for more info on the format."
      :type align-rules-list-type
      :group 'maplev-align)

    ;; Define the alignment exclusion rules.
    ;; The prevent changing quoted material and comments.

    (defcustom maplev-align-exclude-rules-list
      `((exc-dq-string
         (regexp . "\"\\([^\"\n]+\\)\"")
         (repeat . t))
        (exc-sq-string
         (regexp . "'\\([^'\n]+\\)'")
         (repeat . t))
        (exc-bq-string
         (regexp . "`\\([^`\n]+\\)`")
         (repeat . t))
        (exc-open-comment
         (regexp . ,(function
                     (lambda (end reverse)
                       (funcall (if reverse 're-search-backward
                                  're-search-forward)
                                (concat "[^ \t\n\\\\]"
                                        (regexp-quote comment-start)
                                        "\\(.+\\)$") end t))))))
      "A list describing text that should be excluded from alignment.
See the documentation for `align-exclude-rules-list' for more info."
      :type align-rules-list-type
      :group 'maplev-align)))

;;}}}
;;{{{   buffers

(defcustom maplev-pop-up-frames-flag nil
  "Non-nil means help pages and procedure listings start in a separate frame."
  :type 'boolean
  :group 'maplev-misc)

(defcustom maplev-cmaple-end-notice "END_OF_OUTPUT"
  "Message used to indicate the end of Maple output."
  :type 'string
  :group 'maplev-misc)

(defcustom maplev-cmaple-echoes-flag
  (not (string-match "windows-nt\\|ms-dos" (symbol-name system-type)))
  "Non-nil means the process echoes."
  :type 'boolean
  :group 'maplev-buffer
  :group 'maplev-important)

;;}}}
;;{{{   maple setup

(defcustom maplev-start-options (list "-q")
  "List of Maple command line options.  Each item is a string."
  :type 'list
  :group 'maplev-executables)

(defcustom maplev-startup-directory nil
  "If non-nil, change to this directory before running Maple.
Otherwise use the default directory of `maplev-cmaple-buffer'."
  :type '(choice string (const :tag "default" nil))
  :group 'maplev-executables)

;;}}}
;;{{{   help

(defcustom maplev-help-port 3141
  "Port number used to communicate to a Maple help server."
  :type 'integer
  :group 'maplev-help)

(defcustom maplev-help-use-standard-flag nil
  "True means use standard help, if available.
For this to work, Standard Maple must be running Help:-Server,
which is an export of the Help package."
  :type 'boolean
  :group 'maplev-help)

;;}}}
;;}}}

(provide 'maplev-custom)

;;; maplev-custom.el ends here
