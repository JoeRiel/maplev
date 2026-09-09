(require 'maplev)

(ert-deftest maplev--pmaple-buffer-test ()
  (let ((mapledir "/opt/maple2025"))
    (maplev-config
     :mapledir mapledir
     :bindir (concat mapledir "/bin.X86_64_LINUX")
     :maple  (concat mapledir "/bin/maple")
     :maple-options "-B -A2 -e2"
     :pmaple "foo")
    (should (equal
	     (maplev--pmaple-buffer)
	     "Maple (/opt/maple2025/bin/maple)"))))

(ert-deftest maplev-pmaple--process-environment-test ()
  (let ((mapledir "/opt/maple2025")
        process-environment maplev-use-new-language-features)
    (maplev-config
     :mapledir mapledir
     :bindir (concat mapledir "/bin.X86_64_LINUX")
     )
    (should (equal
             (maplev-pmaple--process-environment)
             '("LD_LIBRARY_PATH=/opt/maple2025/bin.X86_64_LINUX:"
               "MAPLE_ROOT=/opt/maple2025")))
    (let ((maplev-use-new-language-features t)
          process-environment)
      (should (equal
               (maplev-pmaple--process-environment)
               '("LD_LIBRARY_PATH=/opt/maple2025/bin.X86_64_LINUX:"
        	 "MAPLE_ROOT=/opt/maple2025"
        	 "MAPLE_NEW_LANGUAGE_FEATURES=1"))))
    (let ((system-type 'windows-nt)
          process-environment maplev-use-new-language-features)
      (should (equal
               (maplev-pmaple--process-environment)
               '("PATH=\"/opt/maple2025/bin.X86_64_LINUX;%PATH%\""
        	 "MAPLE_ROOT=/opt/maple2025"))))
    ))

(ert-deftest maplev-pmaple-default-pmaple-test ()
  (should (equal
    	   (maplev-pmaple-default-pmaple)
    	   "/home/joe/maple/toolbox/maplev/bin.X86_64_LINUX/pmaple"))
  (should (equal
    	   (let ((system-type 'windows-nt))
    	     (maplev-pmaple-default-pmaple))
    	   "/home/joe/maple/toolbox/maplev/bin.X86_64_WINDOWS/pmaple.exe"))
  )

(ert-deftest maplev-pmaple--get-pmaple-and-options-test ()
  (let ((maplev-config (clone maplev-config-default)))
    (set-slot-value maplev-config :maple "maple")
    (should (equal
             (maplev-pmaple--get-pmaple-and-options)
             '("/home/joe/maple/toolbox/maplev/bin.X86_64_LINUX/pmaple"
               "maple" "-B" "-A2" "-e2" "-q"
               "-c maplev:-Setup(\"Maple (maple)\")")))
    ))

(ert-deftest maplev--cleanup-buffer-test ()
  "Test with a buffer with an escape character and a naked carriage return."
  (with-temp-buffer
    (insert "Here is a weird string\e[123m.\r\r")
    (maplev--cleanup-buffer)
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (should (equal content "Here is a weird string.\n")))))

(ert-deftest maplev-pmaple--clear-buffer-test ()
  (let ((mapledir "/opt/maple2025"))
    (cl-letf ((maplev-config (make-instance
                              'maplev-config-class
                              :mapledir mapledir
                              :bindir   (file-name-concat mapledir "bin.X86_64_LINUX")
                              :maple    (file-name-concat mapledir "/bin/maple")
                              )))
      (with-temp-buffer
        (maplev--pmaple-process)
        (maplev-pmaple--clear-buffer)
        (let ((content (buffer-substring-no-properties (point-min) (point-max))))
          (should (equal content "")))))))
