(require 'xtest)
(require 'maplev)

(xt-deftest maplev--ident-around-point-test
  (xt-note "Test maplev--ident-around-point, which returns a string of the identifier at point.")
  (xtd-return= (lambda (default)
		 (with-syntax-table maplev-mode-syntax-table
		   (maplev--ident-around-point default)))
	       ;;("-!-`ab cd`" "`ab cd`")
	       ("-!-foo,bar" "foo")
	       ("f-!-oo,bar" "foo")
	       ("fo-!-o,bar" "foo")
	       ("foo-!-,bar" "foo")
	       ("foo,-!-bar" "bar")
	       ("foo_-!-bar," "foo_bar")
	       ("foo:--!-bar" "bar")

               ;; Is the following desirable?
               ("-!-foo[1]" "foo")
               ("foo[-!-1]" "1")

               ("-!-foo:-bar" "foo")
                                        ; ("foo:--!-bar" "foo:-bar") ; cannot work with design change
	       ("-!-`123`" "`123`")
	       ("`ab-!- cd`" "`ab cd`")
	       ("-!- `ab cd`" "`ab cd`")
	       ("`ab cd` -!-" "`ab cd`")
	       ("-!-`ab" "nada" "nada")
	       ("-!-ab`" "nada" "nada")
	       ("-!-`ab" "nada" "nada")
	       ;; ("ab`-!-")  ; unterminated quoted symbol; need to extend xtd-return
	       ))


(xt-deftest maplev--beginning-of-defun-pos-test
  (xtd-return= (lambda (_)
                 (with-syntax-table maplev-mode-syntax-table
                   (maplev--beginning-of-defun-pos)))
               ("proc()-!- end proc;" 1)
               ("foo := proc()-!- end proc;" 8)
               ("baz; foo := proc()-!- end proc;" 13)
               ("baz; foo := proc() end proc;-!-" 13)
               ("baz; foo := proc() end proc-!-;" 13)

               ("proc() end proc-!-" 1)
               (" proc() end proc-!-" 2)

               ("if proc() end proc()-!-" 4)
               ;; missing proc()q
               ("end proc-!-" nil)

               ;; comments
               ("#proc() end proc-!-" nil)

               ;; multiline comments
               ("proc() (* proc() end proc *) end proc; -!-" 1)
               ("(* proc() *) proc() (* end proc *) end proc; -!-" 14)
               ))

(xt-deftest maplev-beginning-of-defun-test
  (xtd-setup= (lambda(_) (maplev-beginning-of-defun))
              ("proc()-!- end proc;" "-!-proc() end proc;")
              ("proc() end proc;-!-" "-!-proc() end proc;")
              ("foo := proc()-!- end proc;" "foo := -!-proc() end proc;")
              ("baz; foo := proc()-!- end proc;" "baz; foo := -!-proc() end proc;")
              ))

(xt-deftest maplev-end-of-defun-test
  (xtd-setup= (lambda(_) (maplev-end-of-defun))
              ("proc()-!- end proc;" "proc() end proc;-!-")
              ("proc() end proc;-!-" "proc() end proc;-!-")
              ("foo := proc()-!- end proc;" "foo := proc() end proc;-!-")
              ("baz; foo := proc()-!- end proc;" "baz; foo := proc() end proc;-!-")
              ))

(xt-deftest maplev--end-of-defun-pos-test
  (xtd-return= (lambda (_) (maplev--end-of-defun-pos))

               ("proc() -!-end proc;" 17)
               ("proc() end -!-proc;" 17)
               ("end -!-proc;" 10)

	       ("-!-foo := proc() end proc;" 24)
	       ("-!-foo := proc() end proc;\n" 24)
	       ("-!-foo := proc() end proc;# whatever\n" 24)

	       ("foo := proc() end -!-proc;# whatever\n" 24)
	       ("foo := proc() end proc-!-;# whatever\n" 24)
	       ("foo := proc() end proc;-!-# whatever\n" nil)

	       ("foo := proc() end;-!-\n" nil) ; legal maple, but not allowed by maplev--end-of-defun-pos

	       ))

(xt-deftest maplev-mark-defun-test
  (xtd-setup= (lambda (_)
                (maplev-mark-defun))
              ("proc() -!-end proc;" "-!-proc() end proc;")
              ("foo := proc() -!-end proc;" "foo := -!-proc() end proc;")
              ))

(xt-deftest maplev-current-defun-test
  (xtd-return= (lambda (_)
                 (maplev-current-defun))
	       ("proc()-!- end proc;" '(1 17))
	       ("     proc()-!- end proc;" '(6 22))
	       ("baz; proc()-!- end proc;" '(6 22))
	       ("foo := proc()-!- end proc;" '(8 24))
	       ("-!-foo := proc() end proc;" '(8 24))
	       ("foo := proc() end proc;-!-" nil)
	       ("-!-foo := proc() end proc;\n" '(8 25))
	       ("foo := proc() end proc;\nbar := proc() -!-end proc;" '(32 48))
	       ("foo := proc() end proc;\n-!-" nil)
	       ))


(xt-deftest maplev-what-proc-test
  (xtd-return= (lambda (_) (maplev-what-proc 'nodisplay))
               ("foo := proc() -!- end proc;" "foo")
               ("bar := 23; foo := proc() -!- end proc;" "foo")
               ("foo := proc() -!- end proc; bar := proc() end proc" "foo")
               ))


(xt-deftest maplev--beginning-of-defun-test
  (xtd-setup= (lambda(_) (maplev--beginning-of-defun))
              ("proc() end proc;-!-" "-!-proc() end proc;")
              ("\tproc() end proc;-!-" "\t-!-proc() end proc;")
              ))

(xt-deftest maplev--end-of-defun-test
  (xtd-setup= (lambda(_) (maplev--end-of-defun))
              ("-!-proc() end proc; end proc;" "proc() end proc-!-; end proc;")
              ("-!- proc() end proc; end proc;" " proc() end proc-!-; end proc;")
              ("-!- proc() end;\n" " proc() end-!-;\n")
              ))

(xt-deftest maplev--end-of-defun-universal-test
  (xtd-setup= (lambda(_) (universal-argument) (maplev--end-of-defun))
              ("-!-proc() end proc; end proc;"     "proc() end proc-!-; end proc;")
              ("-!- proc() end proc; end proc;"   " proc() end proc-!-; end proc;")
              ("-!- proc() end proc; end proc;\n" " proc() end proc-!-; end proc;\n")
              ("-!- 23; proc() end proc; end proc;\n" " 23; proc() end proc-!-; end proc;\n")
              ))
