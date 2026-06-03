;; (require 'maplev)

(ert-deftest maplev-find-include-file-test-1 ()
  (should (equal
	   (maplev-find-include-file "nada")
	   nil))
  (should (equal
  	   (maplev-find-include-file "nada" (list default-directory))
	   nil))
  (should (equal
  	   (maplev-find-include-file "nada" (list default-directory) 'inc-first)
  	   nil))
  )


