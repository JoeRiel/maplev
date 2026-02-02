(require 'maplev)

;; Test some of the functions used by mint
(ert-deftest maplev-get-option-with-include-test ()
  (cl-letf (((symbol-function 'func) #' maplev-get-option-with-include))
    (should (equal
	     (func
	      (maplev-config :mint-options "")
	      :mint-options
	      "-q")
	     '("-q")))
    (should (equal
	     (func
	      (maplev-config :mint-options "")
	      :mint-options)
	     '()))
    (should (equal
	     (func 
	      (maplev-config :mint-options "" :include-path "")
	      :mint-options)
	     '()))
    (should (equal
	     (func 
	      (maplev-config :mint-options "" :include-path '(""))
	      :mint-options)
	     '()))
    (should (equal
	     (func
	      (maplev-config :mint-options "-q")
	      :mint-options)
	     '("-q")))
    (should (equal
	     (func
	      (maplev-config :mint-options "-q -x")
	      :mint-options)
	     '("-q" "-x")))
    (should (equal 
	     (func
	      (maplev-config :mint-options "" :include-path "/dir")
	      :mint-options)
	     '("-I/dir")))
    (should (equal
	     (func
	      (maplev-config :mint-options "" :include-path '("/dir1" "/dir2"))
	      :mint-options)
	     '("-I/dir1,/dir2")))
    ))






