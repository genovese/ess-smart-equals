;;; ess-smart-equals.el --- better smart-assignment with =-key in R and S  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2019 Christopher R. Genovese, all rights reserved.

;; Author: Christopher R. Genovese <genovese@cmu.edu>
;; Maintainer: Christopher R. Genovese <genovese@cmu.edu>
;; Keywords: R, S, ESS, convenience
;; URL: https://github.com/genovese/ess-smart-equals
;; Version: 0.2.0
;; Package-Version: 0.2.0
;; Package-Requires: ((emacs "25") (ess "16.10"))


;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;


;;; Commentary:
;;
;;  Assignment in R is syntactically complicated by a few features:
;;  1. the historical role of '_' (underscore) as an assignment
;;  character in the S language; 2. the somewhat
;;  inconvenient-to-type, if conceptually pure, '<-' operator as the
;;  preferred assignment operator; 3. the ability to use either an
;;  '=' or an '<-' for assignment; and 4. the multiple roles that
;;  '=' can play, including for setting named arguments in a
;;  function call.
;;
;;  This package gives an alternative smart assignment operator for
;;  R (i.e., S) code that is tied to the '=' key; in fact, it
;;  handles assignment and comparison operators as well as named
;;  argument setting. It uses context in the code to intelligently
;;  guess which operator is intended and then allows very easy
;;  cycling through the possible operators. The contexts and the
;;  operators that are cycled through in each context are
;;  customizable.
;;
;;  The package defines a minor mode `ess-smart-equals-mode',
;;  intended for S-language modes (e.g., ess-r-mode,
;;  inferior-ess-r-mode, and ess-r-transcript-mode), that when
;;  enabled in a buffer activates the '=' key to to handle
;;  context-sensitive completion and cycling of relevant operators.
;;  When the mode is active and an '=' is pressed:
;;
;;   1. In specified contexts (which for most major modes means
;;      in strings or comments), just insert '='.
;; 
;;   2. If an operator relevant to the context lies before point
;;      (with optional whitespace), it is replaced, cyclically, by the
;;      next operator in the configured list for that context.
;;
;;   3. Otherwise, if a prefix of an operator relevant to the
;;      context lies before point, that operator is completed.
;;
;;   4. Otherwise, the highest priority relevant operator is inserted
;;      with surrounding whitespace (see `ess-smart-equals-no-spaces').
;;
;;  Consecutive presses of '=' cycle through the relevant operators.
;;  With a prefix argument, '=' always just inserts an '='.
;;
;;  By default, the minor mode activates the '=' key, but this can
;;  be customized by setting the option `ess-smart-equals-key' before
;;  this package is loaded.
;;
;;  The function `ess-smart-equals-activate' arranges for the minor mode
;;  to be activated by mode hooks for any given list of major modes,
;;  defaulting to ESS major modes associated with R (ess-r-mode,
;;  inferior-ess-r-mode, ess-r-transcript-mode, ess-roxy-mode). 
;;
;;  Examples
;;  --------
;;  In the left column below, ^ marks the location at which an '='
;;  key is pressed, the remaining columns show the result of
;;  consecutive presses of '=' using the package's default settings.
;;  position of point.
;;
;;     Before '='         Press '='      Another '='       Another '='
;;     ----------         ---------      -----------       -----------
;;     foo^               foo <- ^       foo <<- ^         foo = ^
;;     foo  ^             foo  <- ^      foo  <<- ^        foo  = ^
;;     foo<^              foo <- ^       foo <<- ^         foo = ^
;;     foo=^              foo = ^        foo -> ^          foo ->> ^
;;     foo(a^             foo(a = ^      foo( a == ^       foo( a != ^
;;     if( foo=^          if( foo == ^   if( foo != ^      if( foo <= ^
;;     if( foo<^          if( foo < ^    if( foo > ^       if( foo >= ^
;;     "foo ^             "foo =^        "foo ==^          "foo ===^
;;     #...foo ^          #...foo =^     #...foo ==^       #...foo ===^
;;
;;
;;   As a bonus, if `ess-smart-equals-extra-ops' is non-nil when
;;   this package is loaded, this package also binds some other
;;   smart operators that may prove useful. Currently, only
;;   `ess-smart-equals-open-brace' is defined, intended to be bound
;;   to '{'; it configurably places a properly indented and spaced
;;   matching pair of braces at point or around the region if
;;   active. See also `ess-smart-equals-brace-newlines'.
;;
;;   Finally, the primary user facing functions are named with a
;;   prefix `ess-smart-equals-' to avoid conflicts with other
;;   packages. Because this is long, the internal functions and
;;   objects use a shorter (but still distinctive) prefix `essmeq-'.
;;   
;;
;;  Installation and Initialization
;;  -------------------------------
;;  The package can be loaded from MELPA using `package-install' or another
;;  Emacs package manager. Alternatively, you can clone or download the source
;;  directly from the github repository and put the file `ess-smart-equals.el'
;;  in your Emacs load path.
;;
;;  To activate, you need only do
;;
;;      (require 'ess-smart-equals)
;;      (ess-smart-equals-activate)
;;
;;  somewhere in your init file.  and then add `ess-smart-equals-mode' to any
;;  desired mode hooks. For those who use the outstanding
;;  `use-package', you can do
;;
;;      (use-package ess-smart-equals
;;        :after (ess-r-mode)
;;        :config (ess-smart-equals-activate))
;;
;;  somewhere in your init file. An equivalent but less concise version
;;  of this is
;;
;;      (use-package ess-smart-equals
;;        :after (ess-r-mode)
;;        :hook ((ess-r-mode . ess-smart-equals-mode)
;;               (inferior-ess-r-mode . ess-smart-equals-mode)
;;               (ess-r-transcript-mode . ess-smart-equals-mode)
;;               (ess-roxy-mode . ess-smart-equals-mode))
;;               
;;  To also activate the smart brace operator and bind it to '{'
;;  automatically, you can replace this with
;;
;;      (use-package ess-smart-equals
;;        :init   (setq ess-smart-equals-extra-ops '(brace paren))
;;        :after  (ess-r-mode)
;;        :config (ess-smart-equals-activate))
;;
;;  Details on customization are provided in the README file.
;;  

;;; Change Log:
;;
;;  0.2.0 -- Breaking changes in functionality, design, and configuration.
;;           No longer relies on `ess-S-assign' which was deprecated in
;;           ESS. Now provides more powerful context-sensitive, prioritized
;;           operator lists with cycling and completion. The mode is now,
;;           properly, a local minor mode, which can be added automatically
;;           to relevant mode hooks for ESS R modes. Updated required
;;           versions of emacs and ESS.
;;           
;;  0.1.1 -- Initial release with simple insertion and completion, with
;;           space padding for the operators except for a single '=' 
;;           used to specify named arguments in function calls. Relies on
;;           ESS variables `ess-S-assign' and `ess-smart-S-assign-key'
;;           to specify preferred operator for standard assignments.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)

(require 'ess-r-mode)


;;; Key Configuration and Utilities

(defun essmeq--make-transient-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ess-smart-equals-key) #'ess-smart-equals)
    (define-key map "\t" #'essmeq--selected)
    (dolist (key ess-smart-equals-cancel-keys)
      (define-key map key #'essmeq--remove)
      (when (and (or (stringp key) (vectorp key))
                 (= (length key) 1)
                 (not (eq (aref key 0) 7))) ;; Skip C-g
        (define-key map ;; make shift-cancel just do regular backspace
          (vector (if (listp (aref key 0))
                      (cons 'shift (aref key 0))
                    (list 'shift (aref key 0))))
          'delete-backward-char)))
    map))

(defcustom ess-smart-equals-key "="
  "The key for smart assignment operators when `ess-smart-equals-mode' active.

This should either be changed through the customization facility
or before the package is loaded because it affects several
keymaps used by the minor mode."
  :group 'ess-edit
  :type 'string
  :set (lambda (sym value)
         (set-default sym value)
         (setq essmeq--transient-map (essmeq--make-transient-map))
         (define-key ess-smart-equals-mode-map ess-smart-equals-key nil)
         (define-key ess-smart-equals-mode-map value 'ess-smart-equals)))

(defcustom ess-smart-equals-cancel-keys (list [backspace]
                                              (kbd "<DEL>")
                                              (kbd "C-g"))
  "List of keys transiently bound to cancel operator insertion or cycling.
Except for C-g, a shifted version of each will instead delete backwards a 
character, making it easy to delete only part of an operator if desired.

This should either be changed through the customization facility
or before the package is loaded because it affects several
keymaps used by the minor mode."
  :group 'ess-edit
  :type '(repeat (choice string (restricted-sexp :match-alternatives (vectorp))))
  :set (lambda (sym value)
         (set-default sym value)
         (setq essmeq--transient-map (essmeq--make-transient-map))))

(defvar essmeq--transient-map (essmeq--make-transient-map)
  "Map bound transiently after `ess-smart-equals' key is pressed.
The map continues to be active as long as that key is pressed.")

(defun essmeq--keep-transient ()
  "Predicate that returns t when the transient keymap should be maintained."
  (equal (this-command-keys-vector) (vconcat ess-smart-equals-key)))

(defun essmeq--clear-overriding-context ()
  "Transient exit function that resets both itself and any overriding context.
This is a convenience function for fixing a context during one
cycle of smart equals insertion. See
`ess-smart-equals-overriding-context' and
`ess-smart-equals-transient-exit-function'.."
  (setq ess-smart-equals-overriding-context      nil
        ess-smart-equals-transient-exit-function nil))


;;; Behavior Configuration 

(defcustom ess-smart-equals-insertion-hook nil
  "A function called when an operator is inserted into the current buffer.
This does not apply in cases when '=' is inserted literally."
  :group 'ess-edit
  :type 'function)

(defcustom ess-smart-equals-default-modes
  '(ess-r-mode inferior-ess-r-mode ess-r-transcript-mode ess-roxy-mode)
  "List of major modes where `ess-smart-equals-activate' binds '=' by default."
  :group 'ess-edit
  :type '(repeat symbol))

(defcustom ess-smart-equals-extra-ops nil
  "If non-nil, a symbol list of extra smart operators to bind in the mode map.
Currently, only `brace' and `paren' are supported."
  :group 'ess-edit
  :type '(choice (const nil) (repeat (const brace) (const paren))))

(defcustom ess-smart-equals-brace-newlines '((open after)
                                             (close before))
  "Controls auto-newlines for braces in `electric-smart-equals-open-brace'.
Only applicable when `ess-smart-equals-extra-ops' contains the
symbol `brace'. This is an alist with keys `open' and `close' and
with values that are lists containing the symbols `after' and/or
`before', indicating when a newline should be placed. A missing
key is equivalent to a nil value, meaning to place no newlines.

This can be controlled via Emacs's customization mechanism or can
be added to your ESS style specification, as preferred."
  :group 'ess-edit
  :type '(alist :key-type (choice (const open) (const close))
                :value-type (repeat (choice (const before) (const after)))))


;;; Internal Variables (that can be used for advanced customization)

(defvar-local ess-smart-equals-overriding-context nil
  "If non-nil, a context symbol that overrides the usual context calculation.
Intended to be used in a transient manner, see
`ess-smart-equals-transient-exit-function'.")

(defvar-local ess-smart-equals-transient-exit-function nil
  "If non-nil, a nullary function to be called on exit from the transient keymap.
This can be used, for instance, to clear an overriding context.
See `essmeq--transient-map'")

(defvar-local essmeq--stop-transient nil
  "A nullary function called to deactivate the most recent transient map.
This is set automatically and should not be set explicitlyIf non-nil, a nullary function
to be called on exit from the transient keymap. This can be used,
for instance, to clear an overriding context. See
`essmeq--transient-map'.")


;;; Context and Matcher Configuration and Utilities

(defun essmeq--build-matchers (context-alist)
  ""
  (declare (pure t) (side-effect-free t))
  (let (matchers
        (car-or-id (lambda (x) (if (consp x) (car x) x))))
    (dolist (context context-alist (nreverse matchers))
      (push (cons (car context)
                  (essmeq--build-fsm (mapcar car-or-id  (cdr context))
                                     (let* ((info (cdr context))
                                            (data (mapcar #'cdr-safe info)))
                                       (if (cl-every #'null data)
                                           nil
                                         data))))
            matchers))))

(defcustom ess-smart-equals-contexts
  '((t (comment)
       (string)
       (arglist "=" "==" "%>%")
       (index "==" "!=" "<=" "<" ">" ">=" "%in%" "=")
       (conditional "==" "!=" "<=" "<" ">" ">=" "%in%")
       (all "<-" "<<-" "=" "->" "->>"
            "==" "!=" "<" ">" "<=" ">="
            "%<>%" "%>%"
            "+" "*" "/" "%*%" "%%")
       (t "<-" "<<-" "=" "==" "->" "->>" "%<>%"))
    (ess-roxy-mode
     (comment "<-" "=" "==" "<<-" "->" "->>" "%<>%")))
  "Prioritized lists of operator strings for each context and major mode.
This is an alist where each key is either t or the symbol of a
major mode and each value is in turn an alist mapping context
symbols to lists of operator strings in the preferred order.

The mappings for each mode are actually computed by merging the
default (t) mapping with that specified for the mode, with the
latter taking priority.

An empty symbol list for a context means to insert
`ess-smart-equals-key' literally.

If this is changed while the minor mode is running, you will need
to disable and the re-enable the mode to make changes take
effect."
  :group 'ess-edit
  :type '(alist
          :key-type symbol
          :value-type (alist :key-type symbol :value-type (repeat string))))

(defcustom ess-smart-equals-context-function nil
  "If non-nil, a nullary function to calculate the syntactic context at point.
It should return nil, which indicates to fall back on the usual
context calculation, or a symbol corresponding to a context,
i.e., one of the keys in `ess-smart-equals-contexts', either
pre-defined or user-defined. Absent any specific context, the
function can return `t', which is used as a default. When set,
this is called as the first step in the context calculation."
  :group 'ess-edit
  :type 'function)

(defvar-local essmeq--matcher-alist
  (essmeq--build-matchers (map-elt ess-smart-equals-contexts t))
  "Alist mapping context symbols to operator matchers.
Do not set this directly")

(defun ess-smart-equals-set-contexts (&optional mode context-alist)
  (interactive (list (if current-prefix-arg major-mode nil) nil))
  (let ((contexts (or context-alist ess-smart-equals-contexts)))
    (if mode
        (setq essmeq--matcher-alist
              (essmeq--build-matchers
               (map-merge 'list (map-elt contexts t) (map-elt contexts mode))))
      (setq essmeq--matcher-alist
            (essmeq--build-matchers (map-elt contexts t))))))


;;; Utility Macros

(defmacro essmeq--with-struct-slots (type spec-list inst &rest body)
  "Execute BODY with vars in SPEC-LIST bound to slots in struct INST of TYPE.
TYPE is an unquoted symbol corresponding to a type defined by
`cl-defstruct'. SPEC-LIST is a list, each of whose entries can
either be a symbol naming both a slot and a variable or a list of
two symbols (VAR SLOT) associating VAR with the specified SLOT.
INST is an expression giving a structure of type TYPE as defined
by `cl-defstruct', and BODY is a list of forms.

This code was based closely on code given at
www.reddit.com/r/emacs/comments/8pbbpe/why_no_withslots_for_cldefstruct/
which was in turn borrowed from the EIEIO package."
  (declare (indent 3) (debug (sexp sexp sexp def-body)))
  (let ((obj (make-symbol "struct")))
    `(let ((,obj ,inst))
       (cl-symbol-macrolet  ;; each spec => a symbol macro to an (aref ....)
           ,(mapcar (lambda (entry)
                      (let* ((slot-var  (if (listp entry) (car entry) entry))
			     (slot (if (listp entry) (cadr entry) entry))
			     (idx (cl-struct-slot-offset type slot)))
                        (list slot-var `(aref ,obj ,idx))))
                    spec-list)
         (unless (cl-typep ,obj ',type)
	   (error "%s is not of type %s" ',inst ',type))
         ,(if (cdr body) `(progn ,@body) (car body))))))

(defmacro essmeq--with-temporary-insert (text where &rest body)
  "Inserting TEXT after point, execute BODY, delete TEXT.
Returns the value of BODY and does not change point."
  (declare (indent 2) (debug (sexp def-body)))
  (let ((txt (make-symbol "text"))
        (len (make-symbol "text-len"))
        (after (eq where :after)))
    `(let ((,txt ,text)
           (,len ,(if (stringp text) (length text) `(length ,txt))))
       (save-excursion
         ,(if after `(save-excursion (insert ,txt)) `(insert ,txt))
         (prog1 (save-excursion ,@body)
           (delete-char ,(if after len `(- ,len))))))))


;;; Finite-State Machine for Operator Matching
;;
;;  

(cl-defstruct (essmeq-matcher
               (:constructor nil)
               (:constructor essmeq--make-matcher
                             (strings
                              &key
                              (fsm (make-vector
                                    (1+ (apply #'+ (mapcar #'length strings)))
                                    nil))
                              (span (apply #'max 0 (mapcar #'length strings)))
                              (info (make-vector (length strings) nil))
                              (partial nil)
                              &aux
                              (targets (vconcat strings))
                              (data (vconcat info))))
               (:copier essmeq--copy-matcher)
               (:predicate essmeq--matcher-p))
  fsm targets span data partial)

(defun essmeq--build-fsm (ops &optional data)
  "Build backward matching finite-state machine for string vector OPS."
  (declare (pure t) (side-effect-free t))
  (let ((fsm (make-vector (1+ (apply #'+ (mapcar #'length ops))) nil))
        (partial nil)
        (next-state 1) ;; start state 0 always exists
        (max-len 0)
        (num-ops (length ops))
        (op-index 0))
    (while (< op-index num-ops)
      (let* ((state 0)
             (op (elt ops op-index))
             (len (length op))
             (ind (1- len)))
        (when (> len max-len)
          (setq max-len len))
        (while (> ind 0)
          (if-let* ((ch (aref op ind))
                    (in-state (aref fsm state))
                    (goto (assoc ch in-state)))
              (setq state (cadr goto)) ; transition exists, follow it 
            (push (list* ch next-state nil) (aref fsm state)) ; new state
            (when (> state 0)
              (push (cons state (- len ind 1)) (map-elt partial ch))) ; goto for partial match
            (setq state next-state
                  next-state (1+ next-state)))
          (setq ind (1- ind)))
        (if-let* ((ch (aref op 0))
                  (in-state (aref fsm state))
                  (goto (assoc ch in-state)))
            (setf (cddr goto) op-index) ; transition exists, accept it
          (push (list* ch next-state op-index) (aref fsm state)) ; new accept
          (when (> state 0)
              (push (cons state (- len 1)) (map-elt partial ch))) ; goto for partial match
          (setq next-state (1+ next-state))))
      (setq op-index (1+ op-index)))
    (essmeq--make-matcher ops
                          :fsm (cl-map 'vector #'nreverse
                                       (substring fsm 0 next-state))
                          :span max-len
                          :info (if data (vconcat data) nil)
                          :partial (mapcar (lambda (x)
                                             (cl-callf reverse (cdr x))
                                             x)
                                           (nreverse partial)))))

(defun essmeq--match (fsm &optional pos bound)
  "Search backward to exactly match a string specified by machine FSM.
Anchor the search at POS, or at point if nil. BOUND, if non-nil,
limits the search to positions not before position BOUND. Assumes
that surrounding whitespace is handled elsewhere.

Return a dotted list of the form (ACCEPT 0 START . POS) if a match
exists, or nil otherwise. ACCEPT is the number of the accepting
state in FSM, START is the position of the matching string's
beginning, and POS is the position where scanning started, as
passed to this function."
  (let* ((pos (or pos (point)))
         (limit (or bound (point-min)))
         (state 0)
         (accepted nil)
         (start pos))
    (while (and (not (eq state :fail)) (>= start limit))
      (if-let (next (assoc (char-before start) (aref fsm state)))
          (setq state (cadr next)
                accepted (cddr next)
                start (1- start))
        (setq state :fail)))
    (if accepted
        (cl-list* accepted 0 start pos)
      nil)))

(defun essmeq--complete (fsm partial &optional pos bound)
  "Search backward for farthest partial match to a string specified by FSM.
A partial match is a prefix of one of the target operators; the
`farthest' match is the one that moves the position as far back
as possible. Note that this respects the priority order only for
equally far matches.

FSM is the finite-state machine from an `essmeq-matcher'; PARTIAL
is an alist mapping characters to a list of (STATE . SLEN) pairs,
where STATE represents a state to jump to for partial match from POS
and SLEN is the length of the omitted suffix for that partial match.
The search is anchored at POS, or at point if nil. BOUND, if non-nil,
limits the search to positions not before position BOUND. Assumes
that surrounding whitespace is handled elsewhere. 

Return a dotted list of the form (ACCEPT SLEN START . POS) if a
match exists, or nil otherwise. ACCEPT is the number of the
accepting state in FSM, SLEN is the length of the missing suffix
in the partially matched string (0 for full match), START is the
position of the matching string's beginning, and POS is the
position where scanning started, as passed to this function."
  (let* ((pos (or pos (point)))
         (limit (or bound (point-min)))
         (start pos)
         (ch0 (char-before start))
         (skip (copy-sequence (map-elt partial ch0)))
         (state (caar skip))
         (slen (cdar skip))
         (accepted nil)
         (farthest-start (1+ start))
         (farthest-slen 0))
    (when skip
      (pop skip)
      (while (and (not (eq state :fail)) (>= start limit))
        (if-let (next (assoc (char-before start) (aref fsm state)))
            (let ((acc* (cddr next))
                  (farther (< start farthest-start)))
              (when (and acc* farther)
                (setq accepted acc*
                      farthest-start start
                      farthest-slen slen))
              (setq state (cadr next)
                    start (1- start)))
          (if-let ((jump (pop skip)))
              (setq state (car jump)
                    slen (cdr jump)
                    start pos)
            (setq state :fail)))))
    (if accepted
        ;; don't move to failure above so farthest-start off by one
        (cl-list* accepted farthest-slen (1- farthest-start) pos)
      nil)))


;;; Contexts 

(defun essmeq--inside-call-p ()
  "Return non-nil if point is in a function call (or indexing construct).
This is like `ess-inside-call-p' except it also returns true if a closing
parenthesis after point will put point in a call. This is intended to be
used after checking for indexing constructs."
  (or (ess-inside-call-p)
      (essmeq--with-temporary-insert ")" :after (ess-inside-call-p))))

(defun essmeq--context (&optional pos)
  "Compute context at position POS. Returns a context symbol or t.
If `ess-smart-equals-context-function' is non-nil, that function
is called and a non-nil return value is used as the context; a
nil value falls back on the ordinary computation.

There are two known issues here. First, `ess-inside-call-p' does
not detect a function call if the end parens are not closed. This
is mostly fixed by using `essmeq--inside-call-p' instead. Second,
because the R modes characterize % as a string character, a
single % (e.g., an incomplete operator) will cause checks for
function calls or brackets to fail. This can be fixed with a
temporary % insertion, but at the moment, the added complexity
does not seem worthwhile."
  (save-excursion
    (when pos (goto-char pos))
    (cond
     ((and ess-smart-equals-context-function
           (funcall ess-smart-equals-context-function)))
     ((ess-inside-comment-p)  'comment)
     ((let ((closing-char (ess-inside-string-p)))
        (and closing-char (/= closing-char ?%)))
      ;; R syntax table makes % a string character, which we ignore
      'string)
     ((ess-inside-brackets-p) 'index)
     ((essmeq--inside-call-p)
      (if (save-excursion
            (goto-char (ess-containing-sexp-position))
            (or (ess-climb-call-name "if")
                (ess-climb-call-name "while")))
          'conditional
        'arglist))
     (t))))


;;; Processing the Action Key

(defun essmeq--after-whitespace-p (&optional pos)
  (eq (char-syntax (char-before pos)) ?\ ))

(defun essmeq--replace-region (text start &optional end padding)
  (save-excursion
    (goto-char start)
    (when end (delete-region start end))
    (let ((padding (or padding "")))
      (insert padding text padding)
      (point))))

(defun essmeq--search (&optional initial-pos no-partial)
  (let* ((pt (point))
         (pos0 (let ((p (or initial-pos pt)))
                 (save-excursion
                   (when initial-pos (goto-char p))
                   (+ p (skip-syntax-forward " ")))))
         (pos (save-excursion
                (goto-char pos0)
                (+ pos0 (skip-syntax-backward " "))))
         (context (or ess-smart-equals-overriding-context
                      (essmeq--context pos0)))
         (matcher (map-elt essmeq--matcher-alist context)))
    (essmeq--with-struct-slots essmeq-matcher (fsm targets span partial) matcher
      (pcase-let ((`(,accepted ,slen ,start . ,pos1)
                   (or (essmeq--match fsm pos (- pos span))
                       (and (not no-partial)
                            (essmeq--complete fsm partial pos (- pos span)))))
                  (num-ops (length targets)))
        (cond
         (accepted (let* ((op (if (zerop slen)
                                  (mod (1+ accepted) num-ops)
                                accepted))
                          (mtype (if (zerop slen) :exact :partial))
                          (op-string (aref targets op))
                          (ws-start (if (essmeq--after-whitespace-p start)
                                        (1- start)
                                      start)))
                     (list mtype op-string ws-start pos0 " ")))
         ((zerop num-ops) (list :literal "=" pt))
         (t (list :no-match (aref targets 0) pos pos0 " ")))))))

(defun essmeq--process (&optional initial-pos no-partial)
  "Insert, cycle, or complete an appropriate operator based on context."
  (let* ((match (cdr (essmeq--search initial-pos no-partial)))
         (start (cadr match))
         (end (caddr match))
         (pad (and end (>= end (point)))))
    ;;ATTN: still does not handle the initial-pos non-nil case
    (when pad
      (goto-char end)
      (insert " "))
    (apply #'essmeq--replace-region match)
    (when pad
      (delete-char -1))))

(defun essmeq--remove ()
  "Remove an exactly matching operator at point based on context."
  (interactive)
  (let* ((match (essmeq--search nil t))
         (mtype (car match))
         (start (caddr match))
         (end (cadddr match)))
    (if (eq mtype :exact)
        (essmeq--replace-region "" start
                                (if (essmeq--after-whitespace-p (1+ end))
                                    (1+ end)
                                  end))
      (delete-char -1))))

(defun essmeq--selected (op-string)
  "Insert operator string at point with padding, replacing existing operator.
If called interactively, the typical case, select the operator by
completion. If the context operator list is empty, insert
operator string as is."
  (interactive (list (completing-read "Operator: "
                                      (thread-last ess-smart-equals-contexts
                                        (alist-get 't)
                                        (mapcar #'cdr)
                                        (apply #'append)
                                        delete-dups))))
  (let* ((match (essmeq--search nil t))
         (mtype (car match))
         (start (caddr match))
         (end (cadddr match)))
    (if (or (eq mtype :exact) (eq mtype :no-match))
        (let ((end* (if (essmeq--after-whitespace-p (1+ end)) (1+ end) end)))
          (goto-char (essmeq--replace-region op-string start end* " ")))
      (insert op-string))))


;;; Extra Smart Operators

(defun ess-smart-equals-open-brace (&optional literal)
  "Inserts properly indented and spaced brace pair."
  (interactive "P")
  (if literal
      (self-insert-command (if (integerp literal) literal 1))
    (when (not (eq (char-syntax (char-before)) ?\ ))
      (insert " "))
    (let ((pt (point))
          (skeleton-pair t)
          (skeleton-pair-alist '((?\{ "\n" > _ "\n" > ?\}))))
      (skeleton-pair-insert-maybe nil)
      (goto-char pt)
      (ess-indent-exp)
      (forward-char 2)
      (ess-indent-command))))

(defun essmeq--paren-tab ()
  (interactive)
  (when (= (char-after) ?\ ) (delete-char 1))
  (ess-up-list))

(defun essmeq--paren-comma ()
  (interactive)
  (insert ", ")
  (unless (derived-mode-p 'inferior-ess-mode)
    (indent-according-to-mode)))

(defvar essmeq--paren-map (let ((m (make-sparse-keymap)))
                            (define-key m (kbd ",") 'essmeq--paren-comma)
                            (define-key m [?\t] 'essmeq--paren-tab) m)
  "Keymap active in fresh space in the middle of a new smart open paren.")

(defun ess-smart-equals-open-paren (&optional literal)
  "Inserts properly a properly spaced paren pair with an active keymap inside.
ATTN"
  (interactive "P")
  (if literal
      (self-insert-command (if (integerp literal) literal 1))
    (let ((skeleton-pair t)
          (skeleton-pair-alist '((?\(  _ " "
                                       '(let ((pt (point)))
                                          (put-text-property
                                           (1- pt) pt 'keymap essmeq--paren-map))
                                       ?\)))))
      (skeleton-pair-insert-maybe nil))))


;;; Entry Points

;;;###autoload
(defun ess-smart-equals-activate (&rest active-modes)
  "Turn on `ess-smart-equals-mode' in current and future buffers of ACTIVE-MODES.
If non-nil, each entry of ACTIVE-MODES is either a major-mode
symbol or a list of two symbols (major-mode major-mode-hook). In
the former case, the hook symbol is constructed by adding
\"-hook\" to the major mode symbol name. If ACTIVE-MODES is nil,
the specification in `ess-smart-equals-default-modes' is used
instead.

This adds to each specified major-mode hook a function that will
enable `ess-smart-equals-mode' and also enables the minor mode in
all current buffers whose major mode is one of the major modes
just described."
  (interactive)
  (dolist (mode-spec (or active-modes ess-smart-equals-default-modes))
    (let ((mode (if (listp mode-spec) (car mode-spec) mode-spec))
          (hook (if (listp mode-spec)
                    (cdr mode-spec)
                  (intern (concat (symbol-name mode) "-hook")))))
      (add-hook hook #'ess-smart-equals-mode)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (derived-mode-p mode)
            (ess-smart-equals-mode 1)))))))

;;;###autoload
(defun ess-smart-equals (&optional literal)
  "Insert, or substitute, a properly-spaced R (assignment) operator at point.
If an assignment operator is already present before point, it is replaced
by the next operator in `ess-smart-equals-operators', taken cyclically.
The order of these operators is somewhat dependent on context. For instance,
in the argument list of a function call, a single `=' is first rather 
than the standard `<-'; spacing in this case is also determined by
the value of `ess-smart-equals-space-named-arguments'.
For equal signs not preceded by spaces, as in argument lists,
just use equals.  This can effectively distinguish the two uses
of equals in every case.  When RAW is non-nil, the equals sign
is always inserted as is."
  (interactive "P")
  (if (and literal (not (equal literal '(16))))
      (self-insert-command (if (integerp literal) literal 1))
    (when literal
      (message "Cycling over all operators")
      (setq ess-smart-equals-overriding-context  'all
            ess-smart-equals-transient-exit-function #'essmeq--clear-overriding-context))
    (essmeq--process)
    (unless (eq last-command this-command)
      (setq essmeq--stop-transient
            (set-transient-map essmeq--transient-map
                               #'essmeq--keep-transient
                               ess-smart-equals-transient-exit-function))))) 


;;; Minor Mode

(defvar ess-smart-equals-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ess-smart-equals-key 'ess-smart-equals)
    (when ess-smart-equals-extra-ops
      (define-key map "{" 'ess-smart-equals-open-brace))
    map)
  "Keymap used in `ess-smart-equals-mode' binding smart operators.")

;;;###autoload
(define-minor-mode ess-smart-equals-mode
  "Minor mode for setting the '=' key to intelligently handle assignment.

ATTN

With a prefix argument, '=' always just inserts an '='.

Do not set the variable `ess-smart-equals-mode' directly; use the
function of the same name instead."
  :lighter nil
  :keymap ess-smart-equals-mode-map
  (when ess-smart-equals-mode
    (ess-smart-equals-set-contexts ess-smart-equals-contexts major-mode)))


(provide 'ess-smart-equals)

;;; ess-smart-equals.el ends here
