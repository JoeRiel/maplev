(require 'maplev)

;; Test some of the functions used by mint
(ert-deftest maplev-get-option-with-include-test ()
  (cl-letf (((symbol-function 'func) #'maplev-get-option-with-include))
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

;; Test reconfiguring the buffer-local variable maplev-config.

(ert-deftest maplev-config-test ()
    (cl-letf ((maplev-config nil))
      (let ((mapledir "/opt/maple2025"))
        (maplev-config :mapledir mapledir
                       :maple (file-name-concat mapledir "bin" "maple")
                       :mint  (file-name-concat mapledir "bin" "mint")
                       :maple-options "-B -A -e2 -c 'interface(prettyprint=0)'"
                       :bindir nil))
      (should (equal (slot-value maplev-config :maple)  "/opt/maple2025/bin/maple"))
      (should (equal (slot-value maplev-config :mint)   "/opt/maple2025/bin/mint"))
      (should (equal (slot-value maplev-config :bindir) "/opt/maple2025/bin.X86_64_LINUX"))
      ))

