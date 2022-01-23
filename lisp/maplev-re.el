;;; maplev-re.el --- Regular expressions for maplev

;;; Commentary:
;; 

;;; Code:
;;

(defun maplev--list-to-word-re (words)
  "Generate a regular expression that matches one of WORDS, a list."
  (concat "\\<\\(" (regexp-opt words) "\\)\\>"))


(eval-and-compile
  (defconst maplev--declaration-re
    "\\<\\(?:local\\|options?\\|global\\|description\\|export\\|uses\\)\\>"
    "Regular expression for a Maple procedure declaration statement.")

  (defconst maplev--simple-name-re  "\\_<[a-zA-Z_%~][a-zA-Z0-9_?]*\\_>"
    "Regular expression for a simple name.")

  (defconst maplev--quoted-name-re  "`[^`\n\\\\]*\\(?:\\\\.[^`\n\\\\]*\\)*`"
    "Regular expression for a Maple quoted name.
It correctly handles escaped back-quotes in a name, but not
doubled back-quotes.  It intentionally fails for the exceptional
case where a name has a newline character.")

  (defconst maplev--symbol-re (concat "\\(?:"
				      maplev--simple-name-re
				      "\\|"
				      maplev--quoted-name-re
				      "\\)")
    "Regular expression for a Maple symbol.")

  (defconst maplev--name-re
    (concat maplev--symbol-re             ; base name
	    "\\(?:[ \t\n\f]*:-" maplev--symbol-re "\\)*" ; optional module components
	    "\\(?:[ \t\n\f]*\\[[^][]*\\]\\)*" ; optional indices
	    "\\(?:[ \t\n\f]*([^)(]*)\\)*") ; optional arguments
    "Regular expression for Maple names.")

  ;; (defconst maplev--var-with-optional-type (concat
  ;;                                           "\\(" maplev--simple-name-re "\\)"
  ;;                                           "\\(?:\\s-*::\\s-*"
  ;;                                           "\\("
  ;;                                           maplev--type-re
  ;;                                           "\\)\\)?")
  ;;   "Regular expression for a variable with optional type declaration.
  ;; The variable matches group one, the type matches group 2.")


  (defconst maplev--comment-re "#.*$"
    "Regular expression for Maple comments.
A backslash at the end of the line does not continue the comment.")

  (defconst maplev--defun-re "\\(?:\\<proc\\|module\\)\\>"
    "Regular expression at start of a Maple procedure or module.")

  (defconst maplev--assignment-re
    ;; Use "^" to anchor the regular expression.  This forces
    ;; re-search-backward to match the complete assignee name, provided
    ;; that the name is not a split between lines, a very poor practice.
    ;;  (concat "^\\s-*"
    ;;	  "\\(" maplev--name-re "\\)[ \t\n]*:=[ \t\n]*")
    ;;  "Regular expression that matches a Maple assignment.")
    (concat "\\(?:^\\|\\s-\\|[,]\\)"
	    "\\('?" maplev--name-re "'?\\)[ \t\n]*:?=[ \t\n]*")
    "Regular expression that matches a Maple assignment.")

  (defconst maplev--possibly-typed-assignment-re
    (concat "^\\s-*"
	    "\\("
	    "\\(?:\\(?:local\\|global\\|export\\)?\\s-*\\)"
	    "\\('?" maplev--name-re "'?\\)"
	    "\\)"
	    "\\(?:[ \t\n]*::[ \t\n]*\\(" maplev--name-re "\\)\\)?"
	    "[ \t\n]*:?=[ \t\n]*")
    "Regular expression that matches a Maple assignment that may
include a type declaration.  The first group contains the keyword
local, global, or export, if present, and the second group, which
is the assignee.  This third group is the type.  This only works
with an assignment to a name, it does not match an assignment to
a sequence.")

  (defconst maplev--defun-begin-re
    ;; This regular expression does not match a named module,
    ;; nor does it match a procedure/module that is not an
    ;; assignment statement.
    (concat maplev--possibly-typed-assignment-re ;; assignment-re
	    "\\(?:" maplev--comment-re "\\)?"
	    "[ \t\f\n]*" maplev--defun-re)
    "Regular expression for Maple defun assignments.
The second group corresponds to the name of the defun.")

  (defconst maplev--top-defun-begin-re
    (concat "^\\(" maplev--name-re "\\)[ \t\n]*:=[ \t\n]*"
	    "\\(?:" maplev--comment-re "\\)?"
	    "[ \t\f\n]*" maplev--defun-re)
    "Regular expression for top-level Maple defun assignments.
The first group corresponds to the name of the defun.
This requires that the procedure is flush-left.")

  (defconst maplev--defun-end-re
    ;; This regular expression matches any nonqualified end statement,
    ;; such as "do ... end"; however, I consider such code to be bad form
    ;; (with the exception of procedures and modules, which allow it for
    ;; historical reasons).  The proper technique is "do ... end do" or
    ;; "do ... od".
    (concat "\\<end\\>"
	    "\\(?:[ \t]+" maplev--defun-re "\\)?"
	    )
    "Regular expression for \"end\" statement in a Maple defun.
It does not allow line-breaks as this messes up searching.
It matches from the \"end\" to the optional following symbol.")

  (defconst maplev--defun-end-re-colon
    (concat maplev--defun-end-re "[ \t]*[:;]")
    "Regular expression for \"end\" statement in a Maple defun.
It does not allow line-breaks as this messes up searching.
It matches from the \"end\" to the terminating colon or semicolon.")

  (defconst maplev--top-defun-end-re-colon
    (concat "^\\(?:" maplev--defun-end-re-colon "\\)" ; flush left end
	    "\\|"                         ; or
	    maplev--top-defun-begin-re "[^#\n]*" ; one line proc
	    maplev--defun-end-re-colon)
    "Regular expression for \"end\" statement in a top level Maple procedure assignment.
It matches either a flush left \"end\" or a one line procedure assignment.")

  (defconst maplev--space-dot-quote-re "\\s-*\\.[`\"]") ; space could be allowed 'twixt dot and quote

;;;(defconst maplev--quote-re "\"[^\"]*\"\\|`[^`]*`")    ; fails when a quote contains a quote.

  (defconst maplev--string-re "\"[^\"\\\\]*\\(\\\\[[:ascii:]][^\"\\\\]*\\)*\""
    "Regular expression that matches a double-quoted Maple string.
It matches even when a string contains newlines or escaped characters,
including double-quotes.")

  (defconst maplev--quote-re
    (concat maplev--quoted-name-re
	    "\\|"
	    maplev--string-re)
    "Regular expression that matches a backward-quoted name or double code string.")

  (defconst maplev--compile-error-re
    "^on line \\([0-9]+\\) of[ \n]\"\\([^\"]*\\)"
    "Regular expression that matches the output of a Maple load time error message.
This is intended to be assigned to an element of `compilation-error-regexp-alist-alist'.
The first group matches the line number, the second group the file name.")

  (defconst maplev--link-re
    "^#LINK\\s-+\\([^ \t\n]+*\\)"
    "Regular expression that matches a link statement.
The first group is the linked file.")

  (defun maplev--make-suffix-regexp (word)
    "Create a regular expression that matches a suffix of WORD.
For example, given \"begin\" the regular expression matches \"gin\"."
    (let ((re (substring word 0 1)))
      (mapc (lambda (c) (setq re (concat "\\(?:" re "?" (char-to-string c) "\\)")))
	    (substring word 1))
      re))

  (defconst maplev-partial-end-defun-re
    (concat "\\("
	    (maplev--make-suffix-regexp "end")
	    "?\\s-+\\)?\\(?:"
	    (maplev--make-suffix-regexp "proc")
	    "\\|"
	    (maplev--make-suffix-regexp "module")
	    "\\)\\>"))
  "Regular expression that matches a suffix of the end of a procedure or module
assignment; this assumes that a naked \"end\" is not used (may have to rethink
that, as they are used).")


(provide 'maplev-re)

;;; maplev-re.el ends here
