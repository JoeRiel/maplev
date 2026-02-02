(require 'xtest)
(require 'maplev)

(xt-deftest maplev--ident-around-point-test
  (xtd-return= (lambda (default)
		 (with-syntax-table maplev-mode-syntax-table
		   (maplev--ident-around-point default)))
	       ;;("-!-`ab cd`" "`ab cd`")
	       ("-!-foo,bar" "foo")
	       ("foo-!-,bar" "foo")
	       ("foo,-!-bar" "bar")
	       ("foo_-!-bar," "foo_bar")
	       ("foo-!-:-bar" "foo")
	       ("foo:--!-bar" "bar")
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

(xt-deftest maplev--end-of-defun-pos-test
  (xtd-return= (lambda (_) (maplev--end-of-defun-pos))
	       ("-!-foo := proc() end proc;" 24)
	       ("-!-foo := proc() end proc;\n" 24)
	       ("foo := proc() end;-!-\n" nil)
	       ))

(xt-deftest maplev-current-defun-test
  (xtd-return= (lambda (_) (maplev-current-defun))
	       ("foo := proc()-!- end;" '(1 19))
	       ("-!-foo := proc() end;" '(1 19))
	       ("foo := proc() end;-!-" '(1 19)) ; is this correct?
	       ("-!-foo := proc() end;\n" '(1 20))
	       ("foo := proc() end;\nbar := proc() -!-end;" '(20 38))
	       ("foo := proc() end;\n-!-" nil)
	       ))
