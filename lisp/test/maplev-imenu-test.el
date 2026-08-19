(require 'xtest)
(require 'maplev)

(xt-deftest maplev--imenu-create-index-function-test
  (xtd-return= (lambda(_)
                 (maplev-mode)
                 (setq imenu-use-markers nil)
                 (maplev--imenu-create-index-function))
               ("foo := proc() end proc;-!-" `(("Procedures" ("foo" . 1))))
               ("foo := proc() end proc;\n$define FOO foo-!-"
                 `(("$defines" ("FOO" . 25)) ("Procedures" ("foo" . 1))))
               ((concat "  foo := proc() end proc;\n"
                        "$define FOO foo\n"
                        "  bar := module() end module:-!-")
                `(("$defines" ("FOO" . 27))
                  ("Modules" ("bar" . 43))
	          ("Procedures" ("foo" . 1))))
               ))

