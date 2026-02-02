(require 'maplev)
(require 'xtest)

(ert-deftest maplev--number-re-test ()
  (let ((str "0x"))
    (should
     (equal
      (and (string-match maplev--number-re str)
	   (match-string 0 str))
      "0")))
  (let ((str "-0.1x"))
    (should
     (equal
      (and (string-match maplev--number-re str)
	   (match-string 0 str))
      "-0.1")))
  (let ((str "-0.1e-4x"))
    (should
     (equal
      (and (string-match maplev--number-re str)
	   (match-string 0 str))
      "-0.1e-4")))
  )

(xt-deftest maplev--symbol-re-test
  (xtd-should (lambda (str match)
		(string-match maplev--symbol-re str)
		(equal (match-string 0 str) match))
              ("a" "a")
              ("a+b" "a")
              ("a-b" "a")
              ("`a+b`" "`a+b`")
	      ))
    


(xt-deftest maplev-forward-expr-test
  ;; (xt-note "Testing the insert procedure.")
  (xtd-setup= (lambda (dummy)
		(with-syntax-table maplev-mode-syntax-table
		  (maplev-forward-expr)))
              ("-!-%a" "%a-!-")
              ("-!-a+b" "a+b-!-")
              ("-!-(a b)" "(a b)-!-")
              ("-!-a .. b" "a .. b-!-")
              ("-!-a!" "a!-!-")
              ("-!-a++" "a++-!-")
              ("-!-a++" "a++-!-")
	      ("-!-a*b/c^d^3" "a*b/c^d^3-!-")
	      ("-!-a,b,c" "a-!-,b,c")
	      ))
