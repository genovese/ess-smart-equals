;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I turn on ess-smart-equals$"
  (lambda ()
    (ess-smart-equals-mode 1)))

(Given "^I turn off ess-smart-equals$"
  (lambda ()
    (ess-smart-equals-mode -1)))

(When "^I type \"\\(.+\\)\"$"
  "Simulate typing"
  (lambda (typing)
    (execute-kbd-macro (string-to-vector typing))))

(When "^I type '\\(.+\\)'$"
  "Simulate typing"
  (lambda (typing)
    (execute-kbd-macro (string-to-vector typing))))


