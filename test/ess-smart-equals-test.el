(ert-deftest ess-smart-equals/build-fsm-test-0 ()
  "Tests building and matching of reverse-search finite-state machine."
  (let ((ops0  ["<-"])
        (fsm0  [((45 1)) ((60 2 . 0)) nil]))
    (let ((f0 (essmeq--build-fsm ops0)))
      (should (equal (essmeq-matcher-fsm f0) fsm0))
      (should (equal (essmeq-matcher-targets f0) ops0))
      (essmeq--with-struct-slots essmeq-matcher (fsm targets span) f0
        (should (equal fsm fsm0))
        (should (equal targets ops0))
        (should (= span 2))
        (with-temp-buffer
          (insert "a <- b ;;\n123\nZ")
          (should (equal (essmeq--match fsm 5 1) '(0 0 3 . 5)))
          (should (equal (essmeq--match fsm 12 1) nil)))))))

(ert-deftest ess-smart-equals/build-fsm-test-1 ()
  "Tests building and matching of reverse-search finite-state machine."
  (let ((ops1  ["<-" "=" "==" "<<-" "->" "->>" "%<>%"])
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
    (let ((f1 (essmeq--build-fsm ops1)))
     (should (equal (essmeq-matcher-fsm f1) fsm1))
     (should (equal (essmeq-matcher-targets f1) ops1))
     (essmeq--with-struct-slots essmeq-matcher (fsm targets span partial) f1
       (should (equal fsm fsm1))
       (should (equal targets ops1))
       (should (= span 4))
       (with-temp-buffer
         (insert "a <- b;\nc = d;\ne == f;\ng ->> h;\ni %<>% j;\n")
         (should (equal (essmeq--match fsm 5)  '(0 0 3 . 5)))
         (should (equal (essmeq--match fsm 12) '(1 0 11 . 12)))
         (should (equal (essmeq--match fsm 20) '(2 0 18 . 20)))
         (should (equal (essmeq--match fsm 29) '(5 0 26 . 29)))
         (should (equal (essmeq--match fsm 39) '(6 0 35 . 39)))
         (should (equal (essmeq--match fsm 10 1) nil)))
       (with-temp-buffer
         (insert "a < b;\nc - d;\ne = f;\ng -> h;\ni %< j;\n")
         (should (equal (essmeq--complete fsm partial 4)  '(0 1 3 . 4)))
         (should (equal (essmeq--complete fsm partial 11) '(4 1 10 . 11)))
         (should (equal (essmeq--complete fsm partial 18) '(2 1 17 . 18)))
         (should (equal (essmeq--complete fsm partial 26) '(5 1 24 . 26)))
         (should (equal (essmeq--complete fsm partial 34) '(6 2 32 . 34)))
         (should (equal (essmeq--complete fsm partial 6 1) nil)))))))

(ert-deftest ess-smart-equals/inside-call-p-test ()
  "Test modified check of whether we are inside a syntactic call"
  (with-temp-buffer
    (insert "if( z ) x g(y, z, w) h(a,   ")
    (goto-char 5)
    (should (essmeq--inside-call-p))
    (goto-char 9)
    (should-not (essmeq--inside-call-p))
    (goto-char 16)
    (should (essmeq--inside-call-p))
    (goto-char 20)
    (should (essmeq--inside-call-p))
    (goto-char 21)
    (should-not (essmeq--inside-call-p))
    (goto-char 27)
    (should (essmeq--inside-call-p))))

(ert-deftest ess-smart-equals/contexts ()
  "Test modified check of whether we are inside a syntactic call"
  (with-temp-buffer
    (insert "#2345\n")
    (insert "\"89AB\"\n")
    (insert "a\n")
    (insert "if( u )\n")
    (insert "while( u )\n")
    (insert "f( u, v )\n")
    (insert "f() + 2")
    (should (eq (essmeq--context 4) 'comment))
    (should (eq (essmeq--context 9) 'string))
    (should (eq (essmeq--context 15) t))
    (should (eq (essmeq--context 20) 'conditional))
    (should (eq (essmeq--context 31) 'conditional))
    (should (eq (essmeq--context 40) 'arglist))
    (should (eq (essmeq--context) t))
    (let ((ess-smart-equals-context-function (lambda () 'foo)))
      (should (eq (essmew--context) 'foo)))
    (let ((ess-smart-equals-overrriding-context 'bar))
      (should (eq (essmew--context) 'foo)))))

