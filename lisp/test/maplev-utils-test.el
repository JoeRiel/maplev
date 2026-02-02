(require 'maplev)

(ert-deftest maplev--string-to-name-test ()
  (cl-letf (((symbol-function 'func) #'maplev--string-to-name))
    (should (equal (func "a") "a"))
    (should (equal (func "12") "`12`"))
    ))

(ert-deftest maplev-split-shell-option-string-test ()
  (cl-letf (((symbol-function 'func) #'maplev-split-shell-option-string))
    (should (equal
	     (func "")
	     nil))
    (should (equal
	     (func " ")
	     nil))
    (should (equal
	     (func "-a")
	     '("-a")))
    (should (equal
	     (func "-a  -b 3 -c")
	     '("-a" "-b3" "-c")))
    ))

(ert-deftest maplev-add-maple-to-compilation-test ()
  (cl-letf ((compilation-error-regexp-alist compilation-error-regexp-alist)
            (compilation-error-regexp-alist-alist compilation-error-regexp-alist-alist))
    (should-not (member 'maple compilation-error-regexp-alist))
    (maplev-add-maple-to-compilation)
    (should (member 'maple compilation-error-regexp-alist))
    (should (member `(maple ,maplev--compile-error-re 2 1 nil) compilation-error-regexp-alist-alist))
    ))

;; (ert-deftest maplev-edit-source-test ()
;;   (maplev-edit-source "simplify"))
