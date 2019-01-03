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

;;;

(ert-deftest ess-smart-equals/build-fsm-test ()
  "Tests building of reverse-matching finite-state machine."
  (let ((ops0  ["<-"])
        (fsm0  [((45 1)) ((60 2 . 0)) nil])
        (ops1  ["<-" "=" "==" "<<-" "->" "->>" "%<>%"])
        (fsm1  [((?- 1 . nil) ;; state 0
                 (?= 3 . 1)      
                 (?> 6 . nil)
                 (?% 10 . nil))
                ((?< 2 . 0))  ;; state 1
                ((?< 5 . 3))  ;; state 2
                ((?= 4 . 2))  ;; state 3
                nil           ;; state 4
                nil           ;; state 5
                ((?- 7 . 4)   ;; state 6
                 (?> 8 . nil))
                nil              ;; state 7
                ((?- 9 . 5))     ;; state 8
                nil              ;; state 9
                ((?> 11 . nil))  ;; state 10
                ((?< 12 . nil))  ;; state 11
                ((?% 13 . 6))    ;; state 12
                nil]))
    (should (equal (essmeq-matcher-fsm (essmeq--build-fsm ops0)) fsm0))
    (should (equal (essmeq-matcher-fsm (essmeq--build-fsm ops1)) fsm1))))




