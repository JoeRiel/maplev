(require 'maplev)
(require 'xtest)

(xt-deftest maplev--list-to-word-re-test
  (xt-note "Verify maplev--list-to-word-re.")
  (xtd-should (lambda (words str)
                (equal (maplev--list-to-word-re words) str))
              ;; (("") regexp-unmatchable)
              (("") "\\<\\(?:\\)\\>") ; the
              (("a") "\\<a\\>")
              (("ab" "cd" "ab") "\\<\\(?:ab\\|cd\\)\\>")
              ))

;; (maplev--list-to-word-re '())

(xt-deftest maplev--simple-name-re-test
  (xtd-should (lambda (str match)
		(with-syntax-table maplev-mode-syntax-table
		  (and (string-match maplev--simple-name-re str)
		       (equal (match-string 0 str) match))))
	      ("a " "a")
	      ("a1 " "a1")
	      ("a_1 " "a_1")
	      ("a_1+" "a_1")
	      ("a_1-" "a_1")
	      ("a_b1-3" "a_b1")
	      ("%a" "%a")
	      ("a?" "a?")
	      ))

(xt-deftest maplev--quoted-name-re-test
  (xtd-should (lambda (str match)
		(with-syntax-table maplev-mode-syntax-table
		  (and (string-match maplev--quoted-name-re str)
		       (equal (match-string 0 str) match))))
              ("`a` " "`a`")
              ("`1+2`" "`1+2`")
              ("`1+2`+3" "`1+2`")
	      ))

(xt-deftest maplev--name-re-test
  (xtd-should (lambda (str match)
		(with-syntax-table maplev-mode-syntax-table
		  (and (string-match maplev--name-re str)
		       (equal (match-string 0 str) match))))
              ("a[1]" "a[1]")
              ("a[11]" "a[11]")
              ("a(1)" "a(1)") ; why is this a name?
	      ))

(xt-deftest maplev-partial-end-defun-re-test
  (xtd-should (lambda (str match)
		(with-syntax-table maplev-mode-syntax-table
		  (equal
		   (and (string-match maplev-partial-end-defun-re str)
			(match-string 0 str) match)
		   match)))
	      ("end proc" "end proc")
	      ("nd proc" "nd proc")
	      ("d   proc # whatever" "d   proc")
	      ("end procx # whatever" nil)
	      ("end" nil)
	      ("xx" nil)
	      ))

(xt-deftest maplev--make-suffix-regexp-test
  (xtd-should (lambda (word regex)
                (equal (maplev--make-suffix-regexp word) regex))
              ("abc" "\\(?:\\(?:a?b\\)?c\\)")))

(xt-deftest maplev--symbol-re-test
  (xtd-should (lambda (str match)
		(string-match maplev--symbol-re str)
		(equal (match-string 0 str) match))
              ("a" "a")
              ("a+b" "a")
              ("a-b" "a")
              ("`a+b`" "`a+b`")
              ("[a]" "a")
              ("{a,b,c}" "a")
              ("foo(a)" "foo")
              ("foo[a]" "foo")
              ))

(xt-deftest maplev--defun-assignment-re-test
  (xtd-should (lambda (str match)
		(string-match maplev--defun-assignment-re str)
		(equal (match-string 1 str) match))
              ("foo := proc" "foo")
              ("foo := # whatever\n proc" "foo")
              ("foo := module" "foo")
              ))
