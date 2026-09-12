(require 'maplev)
(require 'cl-lib)

(ert-deftest maplev--help-buffer-test ()
  (cl-letf ((maplev-config nil))
    (let ((mapledir "/opt/maple2025"))
      (maplev-config :mapledir mapledir
                     :maple (file-name-concat mapledir "bin" "maple")
                     :mint  (file-name-concat mapledir "bin" "mint")
                     :maple-options "-B -A -e2 -c 'interface(prettyprint=0)'"
                     :bindir nil))

  (should (equal
           (maplev--help-buffer)
           "Maple help (/opt/maple2025/bin/maple)"))))

