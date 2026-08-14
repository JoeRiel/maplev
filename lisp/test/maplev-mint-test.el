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

(xt-deftest maplev--re-search-forward-test-motion
  (xt-note "Testing motion searching forward.")
  (xtd-setup= (lambda(regex)
                (with-syntax-table maplev-mode-syntax-table
                  (maplev--re-search-forward regex nil 'move)))
              ("-!-proc() end proc;"  "proc() end proc-!-;"  "\\<end proc\\>")
              ("proc() -!-end proc;"  "proc() end proc-!-;"  "\\<end proc\\>")
              ("-!-(*proc() end proc;*)" "(*proc() end proc;*)-!-"  "\\<end proc\\>")
              ("-!-proc() end proc;"  "proc() end proc;-!-"  "\\<no match\\>")
              ))

(xt-deftest maplev--re-search-forward-test-return
  (xt-note "Testing return value searching forward.")
  (xtd-return= (lambda(regex)
                 (with-syntax-table maplev-mode-syntax-table
                   (maplev--re-search-forward regex nil 'move)))
               ("-!-proc() end proc;" 16  "\\<end proc\\>")
               ("proc() -!-end proc;" 16  "\\<end proc\\>")
               ("-!-(*proc() end proc;*)" nil "\\<end proc\\>")
               ("-!-proc() end proc;" nil "\\<no match\\>")
               ))

(xt-deftest maplev--re-search-backward-test-motion
  (xt-note "Testing motion searching backward.")
  (xtd-setup= (lambda(regex)
                (with-syntax-table maplev-mode-syntax-table
                  (maplev--re-search-backward regex nil 'move)))
              ("proc() end -!-proc;"  "-!-proc() end proc;"  "\\<proc\\>")
              ("proc() end proc-!-;"  "proc() end -!-proc;"  "\\<proc\\>")
              ("proc() end proc-!-;"  "-!-proc() end proc;"  "\\<no match\\>")
              ("(*proc() end proc;*)-!-"  "-!-(*proc() end proc;*)"  "\\<proc\\>")
              ))

(xt-deftest maplev--re-search-backward-test-return
  (xt-note "Testing return value searching backward.")
  (xtd-return= (lambda(regex)
                 (with-syntax-table maplev-mode-syntax-table
                   (maplev--re-search-backward regex nil 'move)))
               ("proc() -!-end proc;" 1   "\\<proc\\>")
               ("proc() end proc-!-;" 12  "\\<proc\\>")  ; see maplev--beginning-of-func
               ("proc() end proc;-!-" nil "\\<no match\\>")
               ("(*proc() end proc;*)-!-"  nil "\\<proc\\>")
               ))

(xt-deftest maplev-safe-position-test-return
  (xt-note "Testing a search for a safe buffer position (not in a comment).")
  (xtd-return= (lambda(_)
                 (with-syntax-table maplev-mode-syntax-table
                   (maplev-safe-position)))
               ("proc() # -!- comment" 1)
               ("proc() -!-#  comment" 1)
               ("\n#\nproc() -!-#  comment" 2)
               ("\n\n#\nproc() -!-#  comment" 3)
               ("(* \ncomment\n *) proc() -!-" 1)    ; incorrect
               ))

(xt-deftest maplev-safe-position-test-motion
  (xt-note "Testing a search for a safe buffer position (not in a comment).")
  (xtd-setup= (lambda(_)
                (with-syntax-table maplev-mode-syntax-table
                  (goto-char (maplev-safe-position))))
              ("proc() # -!-comment" "-!-proc() # comment" )
              ("proc() -!-# comment" "-!-proc() # comment")
              ("\n#\nproc() -!-# comment" "\n-!-#\nproc() # comment")
              ("\n\n#\nproc() -!-# comment" "\n\n-!-#\nproc() # comment")
              ("(* \ncomment\n *) proc() -!-" "-!-(* \ncomment\n *) proc() ")
              ))

(xt-deftest maplev--statement-terminator-test
  (xt-note "Verify postion after next statement terminator.")
  (xtd-return= (lambda (_)
                 (with-syntax-table maplev-mode-syntax-table
                   (maplev--statement-terminator)))
               ("proc() -!-local x; end proc;" 16)
               ("proc() -!-local x, y; end proc;" 19)))

(ert-deftest maplev--statement-terminator-test-error ()
  (xt-note "Verify error is raised when terminator cannot be found.")
  (should-error
   (with-temp-buffer
     (insert "proc() end proc")
     (goto-char (point-min))
     (maplev--statement-terminator))))

(xt-deftest maplev--goto-declaration-test
  (xt-note "Verify movement after a selected declaration keyword.")
  (xtd-setup= (lambda (keyword)
                (with-syntax-table maplev-mint-mode-syntax-table
                  (maplev--goto-declaration keyword)))
              ("proc() -!-local i, j; end proc"
               "proc() local-!- i, j; end proc"
               "local")

              ("proc() -!-local i, j; global G; end proc"
               "proc() local i, j; global-!- G; end proc"
               "global")

              ("proc() -!-local i, j; global G; option whatever; end proc"
               "proc() local i, j; global G; option-!- whatever; end proc"
               "option")

              ("proc() -!-local i, j; global G; description \"whatever\"; end proc"
               "proc() local i, j; global G; description-!- \"whatever\"; end proc"
               "description")

              ("proc() -!-local i, j; global G; description \"whatever\"; uses LinearAlgebra; end proc"
               "proc() local i, j; global G; description \"whatever\"; uses-!- LinearAlgebra; end proc"
               "uses")
              ))

(xt-deftest maplev-mint-region-test
  (xtd-return= (lambda(_)
                 (with-syntax-table maplev-mode-syntax-table
                   ;; need to assign maplev-config
                   ;; (setq maplev-config maplev-config-default)
                   (maplev-config
                    :mint "mint"
                    :mint-options "-i2"
                    )
                   (maplev-mint-region (point-min) (point-max))
                   (with-current-buffer "*Mint*"
                     (buffer-substring-no-properties (point-min) (point-max)))
                   ))

               ("foo := proc() end proc;" "")

               ;; Check that a number of mint errors are properly detected.

               ((concat "macro(M = Matrix(3),V=Vector(3)):\n"
                        "module M()\n"
                        "export NotUsed, foo;\n"
                        "M[1,1] := 23;\n"
                        "foo := proc(x,x,notused,V)\n"
                        "local i, i, k, u, v, x;\n"
                        "global j, j, _z;\n"
                        "    break;\n"
                        "    seq(kk, kk=1..3);\n"
                        "    1 := NULL;\n"
                        "    v := 23;\n"
                        "    for i to 3 do\n"
                        "        add(i, i=1..2);\n"
                        "        for i to 2 do\n"
                        "            break;\n"
                        "            simplify(`u`*x)\n"
                        "        end do;\n"
                        "    end do;\n"
                        "    2(i);\n"
                        "end proc;"
                        "end module:\n"
                        "return;")
                (concat ""
                        "Nested Procedure foo( x, notused, V ) on lines 5 to 20\n"
                        "  These names were declared more than once as a local variable:  i\n"
                        "  These names appeared more than once in the parameter list:  x\n"
                        "  These names were declared as both a local variable and a parameter:  x\n"
                        "  These variables were used as the same loop variable for nested loops:  i\n"
                        "  These names were used as iteration control variables for both a $, seq, sum, \n"
                        "      or product construct and a loop:  i\n"
                        "  These names were used as global names but were not declared:  kk\n"
                        "  These local variables were never used:  k\n"
                        "  These local variables were used but never assigned a value:  u\n"
                        "  These local variables were assigned a value, but otherwise unused:  v\n"
                        "  These parameters were never used:  V, notused\n"
                        "  These parameters were also used as macro or alias names:  V\n"
                        "  These global variables were declared, but never used:  _z, j\n"
                        "  There is unreachable code following a break statement on line 9\n"
                        "  There is unreachable code following a break statement on line 16\n"
                        "  A break statement is found outside of any loop on line 8\n"
                        "  This expression on line 19 may be missing an operator like *: 2(i)\n"
                        "  Invalid left hand side of assignment on line 10\n"
                        "Module M() on lines 2 to 20\n"
                        "  These exported variables were never used:  NotUsed\n"
                        "  These local variables were also used as macro or alias names:  M\n"
                        "  These local variables were not declared explicitly:  M\n"
                        "A RETURN or return statement is found outside of any procedure on line 21\n"
                        ))
               ))

