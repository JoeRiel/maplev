(require 'maplev)

(ert-deftest maplev-warn-is-enabled-test ()
  (let ((maplev-warn-configuration '((maplev-mode t)
				     (mpldoc-mode (not bar))))
	(maplev-warn-font-lock-feature-keywords-alist
	 '((unequal . maplev-warn-font-lock-unequal-keywords)
	   (foo     . maplev-warn-font-lock-foo-keywords)
	   (bar     . maplev-warn-font-lock-bar-keywords))))
    (should (equal
             (maplev-warn-is-enabled 'maplev-mode) t))
    (should (equal
             (maplev-warn-is-enabled 'maplev-mode 'unequal) t))
    (should (equal
             (maplev-warn-is-enabled 'maplev-mode 'equal) t))
    (should (equal
             (maplev-warn-is-enabled 'mpldoc-mode 'equal) t))
    (should (equal
           (maplev-warn-is-enabled 'mpldoc-mode 'foo) t))
    (should (equal
             (maplev-warn-is-enabled 'mpldoc-mode 'bar) nil))
    (should (equal
             (maplev-warn-is-enabled 'nada-mode) nil))
    ))

