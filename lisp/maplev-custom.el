;;; maplev-custom.el --- Customizable parameters for maplev

;;; Commentary:
;; 

;;; Code:
;;

(require 'align)
(require 'maplev-config)

;;{{{ Group definitions

(defgroup maplev nil
  "Major mode for editing Maple source in Emacs"
  :group 'languages)

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

(defcustom maplev-config-default (maplev-config-class "maplev-config")
  "This variable holds default values for the variable `maplev-config';
both are objects of class `maplev-config-class'."
  :type 'object
  :group 'maplev
  :link '(custom-manual "(maplev)Configuration"))

(defcustom maplev-config-auto-assign t
  "Non-nil auto-assigns some slots of the variable `maplev-config'.
When this variable is non-nil, nil values in slots :mapledir,
:bindir, and :mint of the buffer-local variable `maplev-config'
are automatically assigned by the function `maplev-config',
assuming that the :maple slot is properly assigned and usable."
  :type 'boolean
  :group 'maplev)

;;{{{ (*) comments

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
;;{{{ (*) declarations

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

;;}}}
;;{{{ (*) indentation

(defcustom maplev-indent-level 4
  "Indentation of Maple statements with respect to containing block."
  :type 'integer
  :group 'maplev-indentation)

(defcustom maplev-indent-declaration-level 0
  "Indentation of Maple declarations \(local, global, option, description\)."
  :type 'integer
  :group 'maplev-indentation)

(defcustom maplev-indent-tabs-mode nil
  "Non-nil means indentation can insert tabs."
  :type 'boolean
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
;;{{{ (*) templates

(defcustom maplev-copyright-owner "John Q. Public"
  "Copyright owner inserted in the copyright string by `maplev--template-proc-module'."
  :type 'string
  :group 'maplev-templates)

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
;;{{{ (*) miscellaneous

;; Leading commas

(defcustom maplev-leading-comma-flag t
  "Non-nil means the user prefers leading commas when continuing lines.
Currently this only determines whether advice for `fixup-whitespace'
is activated when `maplev-mode' is executed."
  :type 'boolean
  :group 'maplev-misc)

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
;;{{{ (*) align rules

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
;;{{{ (*) buffers

(defcustom maplev-pop-up-frames-flag nil
  "Non-nil means help pages and procedure listings start in a separate frame."
  :type 'boolean
  :group 'maplev-misc)

;;}}}
;;{{{ (*) maple setup

(defcustom maplev-startup-directory nil
  "If non-nil, change to this directory before running Maple.
Otherwise use the default directory of `maplev-cmaple-buffer'."
  :type '(choice string (const :tag "Use default" nil))
  :group 'maplev-executables)

(defcustom maplev-use-new-language-features nil
  "If non-nil, use the new language features of Maple.
The features enabled are release dependent."
  :type 'boolean
  :group 'maplev-executables)

(defcustom maplev-load-path nil
  "If non-nil, set environment variable LD_LIBRARY_PATH to this value.
This is done when starting the cmaple process."
  :type '(choice string (const :tag "None" nil))
  :group 'maplev-executables)

;;}}}
;;{{{ (*) help

(defcustom maplev-help-port 3141
  "Port number used to communicate to a Maple help server.
Only used if `maplev-help-use-standard-flag' is non-nil."
  :type 'integer
  :group 'maplev-help)

(defcustom maplev-help-use-standard-flag nil
  "Non-nil means use standard help, if available.
For this to work, Standard Maple must be running Help:-Server,
which is an export of the Help package."
  :type 'boolean
  :group 'maplev-help)

;;}}}
;;}}}

(provide 'maplev-custom)

;;; maplev-custom.el ends here
