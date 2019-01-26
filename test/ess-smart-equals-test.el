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
        (with-ess-buffer
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
       (with-ess-buffer
         (insert "a <- b;\nc = d;\ne == f;\ng ->> h;\ni %<>% j;\n")
         (should (equal (essmeq--match fsm 5)  '(0 0 3 . 5)))
         (should (equal (essmeq--match fsm 12) '(1 0 11 . 12)))
         (should (equal (essmeq--match fsm 20) '(2 0 18 . 20)))
         (should (equal (essmeq--match fsm 29) '(5 0 26 . 29)))
         (should (equal (essmeq--match fsm 39) '(6 0 35 . 39)))
         (should (equal (essmeq--match fsm 10 1) nil)))
       (with-ess-buffer
         (insert "a < b;\nc - d;\ne = f;\ng -> h;\ni %< j;\n")
         (should (equal (essmeq--complete fsm partial 4)  '(0 1 3 . 4)))
         (should (equal (essmeq--complete fsm partial 11) '(4 1 10 . 11)))
         (should (equal (essmeq--complete fsm partial 18) '(2 1 17 . 18)))
         (should (equal (essmeq--complete fsm partial 26) '(5 1 24 . 26)))
         (should (equal (essmeq--complete fsm partial 34) '(6 2 32 . 34)))
         (should (equal (essmeq--complete fsm partial 6 1) nil)))))))

(ert-deftest ess-smart-equals/inside-call-p-test ()
  "Test modified check of whether we are inside a syntactic call"
  (with-ess-buffer
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
  (with-ess-buffer
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
      (should (eq (essmeq--context) 'foo)))
    (let ((ess-smart-equals-overriding-context 'bar))
      (should (eq (essmeq--context) 'bar)))))

(defmacro ess-smart-equals-th/replace-region
    (line left right text beg end &rest body )
  "Construct a single `essmeq--replace-region' test."
  (declare (indent 3))
  `(progn
     (erase-buffer)
     (insert "A0123456789ABCDEFGHIJKLM\n")
     (insert "A0123456789ABCDEFGHIJKLM\n")
     (insert "A0123456789ABCDEFGHIJKLM\n")
     (insert "A0        123456789ABCDEFGHIJKLM\n")
     (insert "A0        123456789ABCDEFGHIJKLM\n")
     (insert "A0        123456789ABCDEFGHIJKLM\n")
     (insert "A0  123456789ABCDEFGHIJKLM\n")
     (insert "A0  123456789ABCDEFGHIJKLM\n")
     (insert "A0  123456789ABCDEFGHIJKLM\n")
     (insert "A0123456789ABCDEFGHIJKLM\n")
     (insert "A0123456789ABCDEFGHIJKLM\n")
     (insert "A0123456789ABCDEFGHIJKLM\n")
     (goto-char (point-min))
     (save-excursion
       (goto-char (line-beginning-position ,line))
       (let ((ess-smart-equals-padding-left ,left)
             (ess-smart-equals-padding-right ,right))
         (essmeq--replace-region ,text (+ (point) ,beg) (+ (point) ,end))
         ,@body))))

(ert-deftest ess-smart-equals/replace-region ()
  "Test essmeq--replace-region under various settings."
  (with-ess-buffer
    (ess-smart-equals-th/replace-region 1 'one-space 'one-space
      "<-" 2 2
      (should (looking-at-p ".0 <- 12")))
    (ess-smart-equals-th/replace-region 2 'one-space 'one-space
      "<-" 2 2
      (should (looking-at-p ".0 <- 12")))
    (ess-smart-equals-th/replace-region 1 'some-space 'some-space
      "<-" 2 2
      (should (looking-at-p ".0 <- 12")))
    (ess-smart-equals-th/replace-region 1 'no-space 'no-space
      "<-" 2 2
      (should (looking-at-p ".0<-12")))
    (ess-smart-equals-th/replace-region 1 'none 'none
      "<-" 2 2
      (should (looking-at-p ".0<-12")))
    (ess-smart-equals-th/replace-region 1 'none "ZZZ"
      "<-" 2 2
      (should (looking-at-p ".0<-ZZZ12")))
    (ess-smart-equals-th/replace-region 1 "ZZZ" 'none
      "<-" 2 2
      (should (looking-at-p ".0ZZZ<-12")))
    (ess-smart-equals-th/replace-region 1 "ZZZ" 'one-space
      "<-" 2 2
      (should (looking-at-p ".0ZZZ<- 12")))
    (ess-smart-equals-th/replace-region 1 'one-space (lambda (e ews &optional f)
                                                       (if f
                                                           (cons e (+ e 3))
                                                         (goto-char e)
                                                         (insert "===")))
      "<-" 2 2
      (should (looking-at-p ".0 <-===12")))
    (ess-smart-equals-th/replace-region 1 (lambda (bws b &optional f)
                                                       (if f
                                                           (cons (- b 3) b)
                                                         (goto-char b)
                                                         (insert "===")))
                                        'one-space
      "<-" 2 2
      (should (looking-at-p ".0===<- 12")))
    (ess-smart-equals-th/replace-region 1 'one-space (lambda (e ews &optional f)
                                                       (if f
                                                           (cons e (+ e 3))
                                                         (goto-char e)
                                                         (insert "===")))
      "<-" 2 3
      (should (looking-at-p ".0 <-===23")))
    (ess-smart-equals-th/replace-region 5 'one-space 'one-space
      "<-" 5 5
      (should (looking-at-p ".0 <- 12")))
    (ess-smart-equals-th/replace-region 4 'some-space 'some-space
      "<-" 5 5
      (should (looking-at-p ".0   <-     12")))
    (ess-smart-equals-th/replace-region 4 'no-space 'no-space
      "<-" 5 5
      (should (looking-at-p ".0<-12")))
    (ess-smart-equals-th/replace-region 4 'none 'none
      "<-" 5 5
      (should (looking-at-p ".0   <-     12")))
    (ess-smart-equals-th/replace-region 4 'no-space 'no-space
      "<-" 3 10
      (should (looking-at-p ".0<-12")))
    (ess-smart-equals-th/replace-region 4 'one-space 'no-space
      "<-" 2 10
      (should (looking-at-p ".0 <-12")))
    (ess-smart-equals-th/replace-region 4 'one-space 'one-space
      "<-" 1 11
      (should (looking-at-p "A <- 2")))
    (ess-smart-equals-th/replace-region 5 (lambda (bws b &optional f)
                                            (if f
                                                (cons (- b 3) b)
                                              (goto-char b)
                                              (insert "===")))
                                        'one-space
      "<-" 5 5
      (should (looking-at-p ".0   ===<- 12")))
    ))

