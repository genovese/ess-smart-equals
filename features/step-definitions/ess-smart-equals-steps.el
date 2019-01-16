;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I turn on ess-smart-equals$"
  (lambda ()
    (ess-smart-equals-mode 1)))

(Given "^I turn off ess-smart-equals$"
  (lambda ()
    (ess-smart-equals-mode -1)))

(When "^I enter '\\(.+\\)'$"
  "If action chaining is active, add TYPING to the action chain.
Otherwise simulate typing the string TYPING. Use single-quoted
strings to demarcate argument"
  (lambda (typing)
    (let ((chars (string-to-vector typing)))
      (if espuds-chain-active
          (espuds-add-to-chain chars)
        (execute-kbd-macro chars)))))

(When "^I repeat \"\\(.+\\)\" \\([0-9]+\\) times$"
  "Add to action chain or simulate typing TYPING repeated REPEATS times."
  (lambda (typing repeats)
    (let ((execute (if espuds-chain-active
                       #'espuds-add-to-chain
                     #'execute-kbd-macro))
          (r (string-to-number repeats)))
      (thread-last (if-let ((c (and (= (length typing) 1) (aref typing 0))))
                        (make-string r c)
                      (apply #'concat (make-list r typing)))
        string-to-vector
        (funcall execute)))))
