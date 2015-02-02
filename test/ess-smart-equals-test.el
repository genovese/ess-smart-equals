(ert-deftest ess-smart-equals/strip-standard-test ()
  "Strip leading space tests"
  (should (string-equal (ess-smart-equals--strip-leading-space " foo") "foo")))

(ert-deftest ess-smart-equals/strip-extra-space-test ()
  "Strip leading space tests"
  (should (string-equal (ess-smart-equals--strip-leading-space "   f") "  f"))  
  (should (string-equal (ess-smart-equals--strip-leading-space "foo")  "foo"))
  (should (string-equal (ess-smart-equals--strip-leading-space "")  "")))

(ert-deftest ess-smart-equals/strip-boundary-test ()
  "Strip leading space tests"
  (should (string-equal (ess-smart-equals--strip-leading-space "foo")  "foo"))
  (should (string-equal (ess-smart-equals--strip-leading-space "")  "")))

(ert-deftest ess-smart-equals/restore-standard-test ()
  "Restore leading space tests"
  (should (string-equal (ess-smart-equals--restore-leading-space "foo")  " foo")))

(ert-deftest ess-smart-equals/restore-noop-test ()
  "Restore leading space tests"
  (should (string-equal (ess-smart-equals--restore-leading-space " ff")  " ff")))

(ert-deftest ess-smart-equals/restore-empty-test ()
  "Restore leading space tests"
  (should (string-equal (ess-smart-equals--restore-leading-space "")  "")))




