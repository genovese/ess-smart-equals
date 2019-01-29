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

(Then "^current point should be on a magic space with character \"\\(.+\\)\"$"
  (lambda (ch)
    (cl-assert
     (and (eq (get-text-property (point) 'essmeq--magic-space) t)
          (= (char-after) (aref ch 0)))
     nil
     "Expected current point to be a magic space on char '%s'. %s"
     ch
     (buffer-string))))

(Then "^key \"\\(.+\\)\" should be bound in minor mode map"
  (lambda (binding)
    (cl-assert
     (minor-mode-key-binding (kbd binding))
     nil
     "Expected key '%s' to be bound in current minor-mode map.
      %s"
     (kbd binding)
     (list
      (minor-mode-key-binding "(")
      (alist-get 'ess-smart-equals-mode minor-mode-map-alist)
      ess-smart-equals-mode
      ess-smart-equals-extra-ops
      (progn
        (ess-smart-equals-update-keymaps)
        (minor-mode-key-binding "("))
      (where-is-internal 'ess-smart-equals-open-paren)
      (lookup-key ess-smart-equals-mode-map "(")))))

(Then "^I should see multi-line \"\\([^\"]+\\)\"$"
  "Asserts that the current buffer includes some text."
  (lambda (expected)
    (let ((actual (buffer-string))
          (message "Expected\n%s\nto be part of:\n%s")
          (text (replace-regexp-in-string
                 "[^\\\\]\\(\\\\n\\)" "\n" expected nil nil 1)))
      (cl-assert (s-contains? text actual) nil message text actual))))

(Then "^I should see across lines"
  "Asserts that the current buffer includes some text."
  (lambda (expected)
    (let ((actual (buffer-string))
          (message "Expected\n%s\nto be part of:\n%s"))
      (cl-assert (s-contains? expected actual) nil message expected actual))))

(When "^I inhibit messages$"
  "Inhibits minibuffer messages during subsequent steps"
  (lambda ()
    (setq inhibit-message t)))

(When "^I allow messages$"
  "Enable minibuffer messages during subsequent steps"
  (lambda ()
    (setq inhibit-message nil)))
