(require 'maplev)
(require 'xtest)

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


;; (ert-deftest maplev-get-option-with-include-test-2 ()
;;   (cl-letf (((symbol-function 'func) #'maplev-get-option-with-include))
;;     (should (equal
;;              (func
;;               (maplev-config :mint "")
;;               :mint)
;;              '()))))

;; The actual values come from .emacs, which assigns maplec-config-default

;; (ert-deftest maplev-config-test ()
;;   (let ((default (clone maplev-config-default))
;;         (config maplev-config-default))
;;     (unwind-protect
;;         (progn
;;           ;; (setf (slot-value config :pmaple) "fakepmaple"
;;           ;;       (slot-value config :bindir) "/tmp/bin"
;;           ;;       (slot-value config :maple) "maple"
;;           ;;       )
;;           ;; (should (equal (slot-value config :pmaple)  "fakepmaple"))
;;           ;; (should (equal (slot-value config :bindir) "/tmp/bin"))
;;           ;; (should (equal (slot-value config :compile) nil))
;;           ;; (should (equal (slot-value config :include-path) nil))
;;           ;; (should (equal (slot-value config :maple) "maple"))
;;           ;; (should (equal (slot-value config :mapledir) nil))
;;           ;; (should (equal (slot-value config :maple-options) "-B -A2 -e2"))
;;           ;; ;; (should (equal (slot-value config :mint) nil))
;;           ;; (should (equal (slot-value config :mint-options) "-i2 -q -v")))
;;           )
;;       ;; restore maplev-config-default
;;       (dolist (slot (eieio-class-slots maplev-config-class))
;;         (let ((name (eieio-slot-descriptor-name slot)))
;;           (oset maplev-config-default name (oref name clone))))
;;       )
;;     )
;;   )

;; (slot-value maplev-config-default :pmaple)
;; (slot-value maplev-config-default :bindir)

;; (eieio-class-slots maplev-config-default)


(ert-deftest maplev-config-test ()
  (let ((config (maplev-config :maple "MAPLE" :mint "mint")))
    (should (equal (slot-value config :maple) "MAPLE"))
    (should (equal (slot-value config :mint)  "mint")))
  (let ((config (maplev-config :mint "MINT")))
    (should (equal (slot-value config :mint) "MINT")))
  )
