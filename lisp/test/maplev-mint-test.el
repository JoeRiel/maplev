;; Test some of the functions used by mint

(require 'xtest)
(require 'maplev)


(ert-deftest maplev-get-option-with-include-test ()
  (cl-letf (((symbol-function 'func) #' maplev-get-option-with-include))
    
    (let ((config (maplev-config :include-path '("/dir1" "/dir2") :mint-options "-q")))
      (should (equal (func config :mint-options) '("-q" "-I/dir1,/dir2"))))
  
    (let ((config (maplev-config :include-path '("/dir1" "/dir2") :mint-options "-q -I joe")))
      (should (equal (func config :mint-options) '("-q" "-Ijoe" "-I/dir1,/dir2"))))

    (let ((config (maplev-config :include-path '("/dir1" "/dir2") :mint-options "")))
      (should (equal (func config :mint-options) '("-I/dir1,/dir2"))))

    (let ((config (maplev-config :include-path '("/dir1" "/dir2") :mint-options "-q -v")))
      (should (equal (func config :mint-options) '("-q" "-v" "-I/dir1,/dir2"))))

    (let ((config (maplev-config :include-path '("/dir1" "/dir2") :mint-options "")))
      (should (equal (func config :mint-options "-q") '("-I/dir1,/dir2" "-q"))))

    (let ((config (maplev-config :include-path '("/dir1" "/dir2") :mint-options "")))
      (should (equal (func config :mint-options "-q" "-w 10") '("-I/dir1,/dir2" "-q" "-w 10"))))
    ))



(ert-deftest maplev-split-shell-options-test-1 ()
  (cl-letf (((symbol-function 'func) #' maplev-split-shell-option-string))
  (should (equal (func "") '()))
  (should (equal (func "-a") '("-a")))
  (should (equal (func "-a1") '("-a1")))
  (should (equal (func "-a1   -b") '("-a1" "-b")))
  (should-error (func "a 1 -b")) ; no leading hyphen
  ))

(xt-deftest maplev-mint-forward-declaration-symbol-test
  (xtd-setup= (lambda (_) 
		(with-syntax-table maplev-mode-syntax-table
		  (maplev-mint-forward-declaration-symbol)))
	      ("-!-%a,b" "%a,-!-b")
	      ("-!-a, b" "a, -!-b")
	      ("-!- a, b" " -!-a, b")
	      ("-!-(* *) a , b" "(* *) -!-a , b")
	      ("-!-a\n #\n , b" "a\n #\n , -!-b")
	      ("-!-a,\tb" "a,\t-!-b")
	      ("-!-`1 2`,b" "`1 2`,-!-b")
	      ("-!-_a,b" "_a,-!-b")
	      ("-!-a?,b?" "a?,-!-b?")
	      ("-!-# a\n,b" "# a\n,-!-b")
	      ))

(xt-deftest maplev-delete-vars-test
  (xtd-setup= (lambda (L) (maplev-delete-vars (nth 0 L) 1 (nth 1 L)))
	      ("-!-a := 23, b" "-!-b" (("a") 11))
	      ("-!-a, b" "-!-b" (("a") 5))
	      ("-!-a, b" "-!-a" (("b") 5))
	      ("-!-a, b" "-!-" (("a" "b") 5))

	      ("-!-a::integer, b" "-!-b" (("a") 14))
	      ("-!-a :: {integer,fraction}, b" "-!-b" (("a") 27))
	      
	      ))
	    
