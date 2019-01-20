;;; ess-smart-equals.el --- a flexible, context-sensitive assignment key for R and S  -*- lexical-binding: t; -*-

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
;;  '=', '<-', and a variety of other operators for assignment; and
;;  4. the multiple roles that '=' can play, including for setting
;;  named arguments in a function call.
;;
;;  This package offers a flexible, context-sensitive assignment key
;;  for R and S that is, by default, tied to the '=' key. This key
;;  inserts or completes relevant, properly spaced operators
;;  (assignment, comparison, etc.) based on the syntactic context in
;;  the code. It allows very easy cycling through the possible
;;  operators in that context. The contexts, the operators, and
;;  their cycling order in each context are customizable.
;;
;;  The package defines a minor mode `ess-smart-equals-mode',
;;  intended for S-language modes (e.g., ess-r-mode,
;;  inferior-ess-r-mode, and ess-r-transcript-mode), that when
;;  enabled in a buffer activates the '=' key to to handle
;;  context-sensitive completion and cycling of relevant operators.
;;  When the mode is active and an '=' is pressed:
;;
;;   1. With a prefix argument or in specified contexts (which for
;;      most major modes means in strings or comments), just
;;      insert '='.
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
;;  After an '=', a backspace (or other configurable keys) removes
;;  the last operator and tab offers a choice of operators by completion.
;;  (Shift-backspace will delete one character only and restore the
;;  usual maning of backspace.) See `ess-smart-equals-cancel-keys'.
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
;;   As a bonus, the value of the variable
;;   `ess-smart-equals-extra-ops' when this package is loaded,
;;   determines some other smart operators that may prove useful.
;;   Currently, only `brace' and `paren' are supported, causing
;;   `ess-smart-equals-open-brace' and `ess-smart-equals-open-paren'
;;   to be bound to '{' and '(', respectively. These configurably
;;   places a properly indented and spaced matching pair at point or
;;   around the region if active. See also
;;   `ess-smart-equals-brace-newlines'.
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
;;      (with-eval-after-load 'ess-r-mode
;;        (require 'ess-smart-equals)
;;        (ess-smart-equals-activate))
;;
;;  somewhere in your init file, which will add `ess-smart-equals-mode' to 
;;  a prespecified (but customizable) list of mode hooks.
;;
;;  For those who use the outstanding `use-package', you can do
;;
;;      (use-package ess-smart-equals
;;        :after (:any ess-r-mode inferior-ess-r-mode ess-r-transcript-mode)
;;        :config (ess-smart-equals-activate))
;;
;;  somewhere in your init file. An equivalent but less concise version
;;  of this is
;;
;;      (use-package ess-smart-equals
;;        :after (:any ess-r-mode inferior-ess-r-mode ess-r-transcript-mode)
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
;;        :after  (:any ess-r-mode inferior-ess-r-mode ess-r-transcript-mode)
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

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))
(require 'map)
(require 'skeleton)

(require 'ess-r-mode)


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

(defmacro essmeq--with-matcher (spec-list inst &rest body)
  "Execute BODY with vars in SPEC-LIST bound to slots essmeq-matcher INST.
SPEC-LIST is a list, each of whose entries can either be a symbol
naming both a slot and a variable or a list of two symbols (VAR
SLOT) associating VAR with the specified SLOT. INST is an
expression giving a structure of type essmeq-matcher, and BODY is
a list of forms."
  (declare (indent 2) (debug (sexp sexp def-body)))
  `(essmeq--with-struct-slots essmeq-matcher ,spec-list ,inst
     ,@body))

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

(defmacro essmeq--with-markers (specs &rest body)
  "Execute BODY with markers defined by SPEC. Markers cleared after BODY forms.
Return the value of the last form in BODY. Markers are guaranteed
to be cleared even if BODY exits non-locally. Note that as a
consequence, the markers themselves should not be returned. If
any of the markers is a desired value of this form, either
`essmeq-copy-marker' or `copy-marker' should be used, but note
that unlike the former, the latter does not copy the insertion
type of the marker by default.

SPEC is a list whose elements must have one of the following
forms: 1. SYMBOL; 2. (SYMBOL POSITION), where POSITION is an
expression used to initialize the marker as in the corresponding
argument to `set-marker'; or 3. (SYMBOL POSITION INSERTION-TYPE),
were INSERTION-TYPE is either t or nil (the default) as in
`set-marker-insertion-type'.

If POSITION is one of the forms (point), (point-min),
or (point-max), those expressions are not evaluated but instead
the marker is created with the corresponding `point-marker',
`point-min-marker', or `point-max-marker', respectively."
  (declare (indent 1) (debug (sexp def-body)))
  (let ((marker-symbols (mapcar (lambda (s) (if (listp s) (car s) s)) specs))
        (marker-info (mapcar (lambda (s) (if (listp s) (cadr s) nil)) specs))
        (marker-itypes
         (delq nil (mapcar (lambda (s)
                             (if (and (listp s) (caddr s))
                                 (cons (car s) (caddr s))
                               nil))
                           specs))))
    (unless (cl-every #'symbolp marker-symbols)
      (error "Only symbols allowed in first entry of marker specification."))
    `(let (,@(cl-map 'list (lambda (s i)
                             (cond
                              ((equal i '(point))
                               `(,s (point-marker)))
                              ((equal i '(point-min))
                               `(,s (point-min-marker)))
                              ((equal i '(point-max))
                               `(,s (point-max-marker)))
                              ((null i)
                               `(,s (make-marker)))
                              (t
                               `(,s (set-marker (make-marker) ,i)))))
                     marker-symbols marker-info))
       ,@(mapcar (lambda (s) `(set-marker-insertion-type ,(car s) ,(cdr s)))
                 marker-itypes)
       (unwind-protect ,(if (cdr body) `(progn ,@body) (car body))
         ,@(mapcar (lambda (m) `(set-marker ,m nil)) marker-symbols)))))


;;; Utility Functions

(defun essmeq--replace-region (text start &optional end padding)
  (save-excursion
    (goto-char start)
    (when end (delete-region start end))
    (let ((padding (or padding "")))
      (insert padding text padding)
      (point))))

(defun essmeq--after-whitespace-p (&optional pos)
  (eq (char-syntax (char-before pos)) ?\ ))

(defun essmeq-make-marker (&optional position type init)
  "Like `make-marker' but also optionally initializes POSITION and TYPE.
POSITION can be any value of the same argument in `set-marker'.
TYPE is nil or t, as with the corresponding argument to
`set-marker-insertion-type'. INIT, if non-nil, should be nullary
function (e.g,. point-marker) to be called instead of `make-marker'
to initialize the marker

Returns the initialized marker."
  (let ((m (if init (funcall init) (make-marker))))
    (when position (set-marker m position))
    (when type (set-marker-insertion-type m t))
    m))

(defun essmeq-copy-marker (&optional marker type)
  "Like `copy-marker' but copies insertion type if MARKER but not TYPE is given."
  (copy-marker marker (or type (and marker (marker-insertion-type marker)))))

(defsubst essmeq-clear-marker (marker)
  "Reset MARKER so that it points nowhere and does not affect current buffer."
  (set-marker marker nil))


;;; Finite-State Machine for Operator Matching
;;
;;  We do backwards anchored matching of operator lists using a
;;  pre-built finite-state machine. This offers several advantages
;;  over a direct sequence of regular expression matches. First, in
;;  benchmarks with compiled code, the FSM matcher gives comparable,
;;  though typically better, performance than the regexp approach.
;;  Second, backwards regex matching in emacs (excluding
;;  looking-back, which is slow) does not give the longest match,
;;  requiring disambiguation between say '<-' and '<<-'. Third, we
;;  can handle partial matches automatically with the information
;;  computed at fsm build time, making it easy to offer completion.
;;  Fourth, we can control priority order in the match easily and
;;  can associate additional information with the matched operator.
;;  Note that the search is backward, so the FSM matching starts
;;  in state 0 at the *end* of the strings.
;;
;;  Each FSM is reepresented by an `essmeq-matcher' object (a struct).
;;  This has several slots:
;;
;;     :fsm The finite state machine. Each state is either nil or an
;;          alist mapping characters to transitions of the form
;;          (CHAR NEXT-STATE ACCEPTED?) where ACCEPTED? is either
;;          nil when the transition is not to an accepting state
;;          or an index into the target string vector when it is.
;;          Note that acceptance is signaled on the *transition*
;;          not in the state itself, so many accepting states are
;;          often nil. Because the search is anchored, a state
;;          can only accept zero or one strings and there can
;;          be multiple accepting states along the path.
;;
;;     :targets The vector of strings being matched, without padding.
;;
;;     :span The maximum length of the target strings
;;
;;     :data A vector, the same length as targets, of optional
;;           associated data.
;;
;;     :partial A table of links that can be used for computing
;;              partial matches. Each partial element is a list of
;;              the form (CHAR (STATE . SLEN)), where STATE is a
;;              candidate STATE to start in for finding the partial
;;              match and SLEN is the number of skipped characters
;;              at the end of the string for that partial match.
;;
;;  These matchers are built with `essmeq--build-fsm' and matched
;;  with `essmeq--match' (for exact matches) and `essmeq--complete'
;;  (for partial matches).

(cl-defstruct (essmeq-matcher
               (:constructor nil)
               (:constructor essmeq-make-matcher
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
               (:copier essmeq-copy-matcher)
               (:predicate essmeq-matcher-p))
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
            (when (> state 0)  ; goto for partial match
              (push (cons state (- len ind 1)) (map-elt partial ch))) 
            (setq state next-state
                  next-state (1+ next-state)))
          (setq ind (1- ind)))
        (if-let* ((ch (aref op 0))
                  (in-state (aref fsm state))
                  (goto (assoc ch in-state)))
            (setf (cddr goto) op-index) ; transition exists, accept it
          (push (list* ch next-state op-index) (aref fsm state)) ; new accept
          (when (> state 0)   ; goto for partial match
              (push (cons state (- len 1)) (map-elt partial ch)))
          (setq next-state (1+ next-state))))
      (setq op-index (1+ op-index)))
    (essmeq-make-matcher ops
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

(defun essmeq--fallback (pos)
  "Fallback completion at pos ATTN; what will this do in strings and comments"
  (when-let ((matcher (map-elt essmeq--matcher-alist 'base)))
    (essmeq--with-matcher (fsm targets span partial) matcher
      (essmeq--complete fsm partial pos (- pos span)))))


;;; Key Configuration and Utilities

(defcustom ess-smart-equals-key "="
  "The key for smart assignment operators when `ess-smart-equals-mode' active.

If this is changed after `ess-smart-equals-mode' has been enabled
in a buffer, then either the mode should be disabled and
re-enabled in one such buffer or you should do

   M-x ess-smart-equals-update-keymaps

in order for this change to take effect."
  :group 'ess-edit
  :type 'string)

(defcustom ess-smart-equals-extra-ops nil
  "If non-nil, a symbol list of extra smart operators to bind in the mode map.
Currently, only `brace' and `paren' are supported.

If this is changed after `ess-smart-equals-mode' has been enabled
in a buffer, then either the mode should be disabled and
re-enabled in one such buffer or you should do

   M-x ess-smart-equals-update-keymaps

in order for this change to take effect."
  :group 'ess-edit
  :type '(choice (const nil) (repeat (const brace) (const paren))))

(defcustom ess-smart-equals-cancel-keys (list [backspace]
                                              (kbd "<DEL>")
                                              (kbd "C-g"))
  "List of keys transiently bound to cancel operator insertion or cycling.
Except for C-g, a shifted version of each will instead delete backwards a 
character, making it easy to delete only part of an operator if desired.

If this is changed after `ess-smart-equals-mode' has been enabled
in a buffer, then either the mode should be disabled and
re-enabled in one such buffer or you should do

   M-x ess-smart-equals-update-keymaps

in order for this change to take effect."
  :group 'ess-edit
  :type '(repeat
          (choice string (restricted-sexp :match-alternatives (vectorp)))))

(defun essmeq--make-transient-map (&optional cancel-keys)
  "Resets transient keymap used after `ess-smart-equals'.
CANCEL-KEYS, if non-nil, is a list of keys in that map that will
clear the last insertion. It defaults to
`ess-smart-equals-cancel-keys', which see. See also
`essmeq--transient-map'."
  (let ((cancel-keys (or cancel-keys ess-smart-equals-cancel-keys))
        (map (make-sparse-keymap)))
    (define-key map (kbd ess-smart-equals-key) #'ess-smart-equals)
    (define-key map "\t" #'essmeq--selected)
    (dolist (key cancel-keys)
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

(defun essmeq--make-mode-map ()
  "Returns the `ess-smart-equals-mode' keymap using current parameter values."
  (let ((map (make-sparse-keymap)))
    (define-key map ess-smart-equals-key 'ess-smart-equals)
    (when (memq 'brace ess-smart-equals-extra-ops)
      (define-key map "{" 'ess-smart-equals-open-brace))
    (when (memq 'paren ess-smart-equals-extra-ops)
      (define-key map "(" 'ess-smart-equals-open-paren))
    map))

(defvar ess-smart-equals-mode-map (essmeq--make-mode-map)
  "Keymap used in `ess-smart-equals-mode' binding smart operators.")

(defvar essmeq--transient-map (essmeq--make-transient-map)
  "Map bound transiently after `ess-smart-equals' key is pressed.
The map continues to be active as long as that key is pressed.")

(defun emacs-smart-equals-update-keymaps ()
  "Force update of `ess-smart-equals-mode' keymaps to adjust for config changes.
This should not usually need to be done explicitly by the user."
  (interactive)
  (setq essmeq--transient-map (essmeq--make-transient-map)
        ess-smart-equals-mode-map (essmeq--make-mode-map)))

(defun essmeq--keep-transient ()
  "Predicate that returns t when the transient keymap should be maintained."
  (equal (this-command-keys-vector) (vconcat ess-smart-equals-key)))


;;; Behavior Configuration 

(defvar ess-smart-equals-padding-left 'one-space
  "ATTN:Convert to defcustom once solidified")
  ;; padding options for left and right
  ;; no-space, one-space, at-least-one-space, "literal string",
  ;; nil (or any other value) means to leave the space as is

(defvar ess-smart-equals-padding-right 'one-space
  "ATTN:Convert to defcustom once solidified")

(defcustom ess-smart-equals-insertion-hook nil
  "A function called when an operator is inserted into the current buffer.
This (non-standard hook) function should accept six arguments 

       CONTEXT MATCH-TYPE STRING START OLD-END PAD

where CONTEXT is a context symbol, representing a key in the
inner alists of `ess-smart-equals-contexts'; MATCH-TYPE is one of
the keywords :exact, :partial, :no-match, or :literal; STRING is
the operator string that was inserted; START is the buffer
positions at the beginning of the inserted string (plus padding);
OLD-END was the ending position of the previous content in the
buffer; and PAD is a string giving the padding used on either
side of the inserted operator, typically either empty or a single
space.

This feature is experimental and may be removed in a future version."
  :group 'ess-edit
  :type 'function)

(defcustom ess-smart-equals-default-modes
  '(ess-r-mode inferior-ess-r-mode ess-r-transcript-mode ess-roxy-mode)
  "List of major modes where `ess-smart-equals-activate' binds '=' by default."
  :group 'ess-edit
  :type '(repeat symbol))

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


;;; Specialized overriding context and transient exit functions

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
This is set automatically and should not be set explicitly. If
non-nil, a nullary function to be called on exit from the
transient keymap. This can be used, for instance, to clear an
overriding context if something goes awry. See
`essmeq--transient-map'.")

(defun ess-smart-equals-clear-overriding-context ()
  "Transient exit function that resets both itself and any overriding context.
This is a convenience function for fixing a context during one
cycle of smart equals insertion. See
`ess-smart-equals-overriding-context' and
`ess-smart-equals-transient-exit-function'.."
  (setq ess-smart-equals-overriding-context      nil
        ess-smart-equals-transient-exit-function nil))

(defun ess-smart-equals-set-overriding-context (context)
  "Force context to be symbol CONTEXT for next insertion only.
This sets `ess-smart-equals-transient-exit-function' to clear the context
the next time the transient map in `ess-smart-equals' exits."
  (setq ess-smart-equals-overriding-context  context
        ess-smart-equals-transient-exit-function
          #'ess-smart-equals-clear-overriding-context))


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

(defcustom ess-smart-equals-mode-updates
  '((inferior-ess-r-mode
     (ess-smart-equals-wrapping-function . essmeq--comint-narrow)))
  "Mode-specific updates of `ess-smart-equals-mode' options.
This is an alist mapping major mode (symbols) to an alist of
option settings that will supersede the default settings when
that mode is in effect. Only options that need to be changed from
their default value need to be included, and only major modes
where such a change is made. These settings take effect in a
buffer when the minor mode is enabled, so after any changes in
this variable, the mode needs to be toggled twice for the changes
to take effect."
  :group 'ess-edit
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type sexp)))

;; ATTN: Allow `:cycle' in context lists. Everything before is a candidate
;; for exact matching/cycling; everything after can be completed only.
;; Need to add `max-cycle' slot to essmeq-matcher structure, which
;; is useful in several places in *--search anyway. Also consider
;; the possibility of including symbols in this list, were these are
;; nullary functions that splice into the targets vector at that point,
;; computed at mode enable when the matchers are created. Use case for
;; this: querying R for names of %infix% operators.
(defcustom ess-smart-equals-contexts
  '((t (comment)
       (string)
       (arglist "=" "==" "!=" "<=" ">=" "<-" "<<-" "%>%")
       (index "==" "!=" "%in%" "<" "<=" ">" ">=" "=")
       ;; This order of inequalities makes cycling align with completion
       (conditional "==" "!=" "<" "<=" ">" ">=" "%in%")
       ;; base holds all operators that are assignment or complete with '='
       (base "<-" "<<-" "=" "==" "!=" "<=" ">=" "->" "->>" ":=")
       ;; Used for smart %-completion and cycling
       (% "%%" "%/%" "%*%" "%in%" "%>%" "%<>%" "%o%" "%x%")
       (all "<-" "<<-" "=" "->" "->>"
            "==" "!=" "<" "<=" ">" ">=" 
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
this is called as the first step in the context calculation. This
function has access to `ess-smart-equals-overriding-context' and
can choose to respect it (by returning it or nil if set) or
ignore it. That variable is next in priority in determining the
context."
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
does not seem worthwhile. Note similarly that when
`ess-inside-string-p' returns a ?%, we could use the % context to
limit to matches to the %-operators."
  (save-excursion
    (when pos (goto-char pos))
    (cond
     ((and ess-smart-equals-context-function
           (funcall ess-smart-equals-context-function)))
     (ess-smart-equals-overriding-context)
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

;;ATTN: experimental example of reading current % operators from R
;;      for use in contexts if we have a function
;;      Note: should replace the replace-* calls later but for now
;;      this is a proof of concept; can actually just search for
;;      the "(%.*%)" and accumulate and nreverse the list of them
;;      this also illustrates how to interact with the r process
;;      when needed
(defun essmeq--percent-operators ()
  ""
  (let ((proc (if (derived-mode-p 'inferior-ess-mode)
                  (get-buffer-process (current-buffer))
                (ess-get-next-available-process)))
        (cmd (format
              "unique(sort(%s))\n"
              "unlist(Map(function(s){ls(s, pattern='%.*%')}, search()))")))
    (if (not proc)
        '("default list ATTN")
      (with-temp-buffer
        (ess-command cmd (current-buffer) nil nil nil proc)
        (goto-char (point-min))
        (replace-regexp "\\(?:^[^\"]*\"%\\|%\" *$\\)" "%")
        (replace-regexp "%\"  *\"%" "% %" nil (point-min) (point-max))
        (replace-string "\n" " " nil (point-min) (point-max))
        (split-string
         (buffer-substring-no-properties (point-min) (point-max)))))))



;;; Processing the Action Key

(defun essmeq--whitespace-span (pos &optional backward)
  "Scan from POS to the end of contiguous whitespace.
The scan is forward unless BACKWARD is non-nil. For the forward scan only,
stop upon encountering a character with a non-nil `essmeq--magic-space'
property.

Return (START . END) positions for the scanned sequence."
  (save-excursion
    (goto-char pos)
    (if backward
        (progn
          (skip-syntax-backward " ")
          (cons (point) pos))
      (skip-syntax-forward " ")
      (let* ((after-ws (point))
             (magic-pos (text-property-any pos after-ws 'essmeq--magic-space t)))
        (cons pos (or magic-pos after-ws))))))

(defun essmeq--normalize-padding (beg end)
  "Adjust space padding on either side of BEG and END in the current buffer.
The spaces used are determined by the values of the options
`ess-smart-equals-padding-left' and
`ess-smart-equals-padding-right', which see."
  (essmeq--with-markers ((mbeg beg t) (mend end))
    (pcase-let ((`(,start-ws . ,end-ws) (essmeq--whitespace-span end)))
      (cond
       ((eq ess-smart-equals-padding-left 'one-space)
        (delete-region start-ws end-ws)
        (save-excursion (goto-char mend) (insert " ")))
       ((eq ess-smart-equals-padding-left 'at-least-one-space)
        (unless (> end-ws start-ws)
          (save-excursion (goto-char mend) (insert " "))))
       ((eq ess-smart-equals-padding-left 'no-space)
        (delete-region start-ws end-ws))
       ((stringp ess-smart-equals-padding-left)
        (save-excursion (goto-char mend) (insert ess-smart-equals-padding-left)))
       (t)))
    (pcase-let ((`(,start-ws . ,end-ws) (essmeq--whitespace-span beg 'backward)))
      (save-excursion
        (goto-char mbeg)
        (cond
         ((eq ess-smart-equals-padding-right 'one-space)
          (delete-region start-ws end-ws)
          (insert " "))
         ((eq ess-smart-equals-padding-right 'at-least-one-space)
          (unless (> end-ws start-ws)
            (insert " ")))
         ((eq ess-smart-equals-padding-right 'no-space)
          (delete-region start-ws end-ws))
         ((stringp ess-smart-equals-padding-right)
          (insert ess-smart-equals-padding-right))
         (t))))
    (cons (essmeq-copy-marker mbeg t) (essmeq-copy-marker mend))))

(defun essmeq--search (&optional initial-pos no-partial)
  "Search backwards for an operator matching the current context.
Search is anchored at INITIAL-POS, or point if nil. If NO-PARTIAL
is nil, then partial matches of a prefix of relevant operators
strings are allowed. Returns a list (CONTEXT MTYPE STRING START
END PADDING), where CONTEXT is a context symbol in
`ess-smart-equals-contexts'; MTYPE is a keyword among :exact,
:partial, :literal (for literal '=' insertion), and :no-match;
STRING is the operator string to be inserted, replacing the
region between START and END. END is non-nil unless MTYPE is
:literal. Finally, PAD is the string padding the actual operator
on both sides, usually either a single space or an empty string."
  (let* ((pt (or initial-pos (point)))
         (pos0 (save-excursion
                 (when initial-pos (goto-char pt))
                 (+ pt (skip-syntax-forward " "))))
         (pos (save-excursion
                (goto-char pos0)
                (+ pos0 (skip-syntax-backward " "))))
         (context (essmeq--context pos0))
         (matcher (map-elt essmeq--matcher-alist context)))
    (essmeq--with-struct-slots essmeq-matcher (fsm targets span partial) matcher
      (pcase-let ((`(,accepted ,slen ,start . ,_)
                   (or (essmeq--match fsm pos (- pos span))
                       (and (not no-partial) ;;ATTN: add fall back complete to 'viable context
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
                     (list context mtype op-string ws-start pos0 " ")))
         ((zerop num-ops) (list context :literal "=" pt))
         (t (list context :no-match (aref targets 0) pos pos0 " ")))))))

(defun essmeq--process (&optional no-partial)
  "Insert, cycle, or complete an operator at point based on context.
Point ends up at the end of the inserted string. Calls
`ess-smart-equals-insertion-hook' on the result of the
search (see `essmeq--search') if the hook is non-nil."
  (let* ((match (essmeq--search (point) no-partial))
         (spec (cddr match)))
    (goto-char (apply #'essmeq--replace-region spec))
    (when ess-smart-equals-insertion-hook
      (apply ess-smart-equals-insertion-hook spec))))

(defun essmeq--remove ()
  "Remove a matching operator at point based on context, else one character."
  (interactive)
  (let* ((match (cdr (essmeq--search nil t)))
         (mtype (car match))
         (start (caddr match))
         (end (cadddr match)))
    (if-let (((eq mtype :exact))
             (end+ws (1+ end))) ;; end not nil in :exact case
        (essmeq--replace-region
         "" start (if (essmeq--after-whitespace-p end+ws) end+ws end))
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
  (let* ((match (cdr (essmeq--search nil t)))
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

(defun essmeq--paren-escape ()
  "Escape paren pair, deleting magic space if starting there."
  (interactive)
  (when (= (char-after) ?\ ) (delete-char 1))
  (ess-up-list))

(defun essmeq--paren-comma ()
  "Insert spaced comma, keeping point on magic space."
  (interactive)
  (insert ", ")
  (unless (derived-mode-p 'inferior-ess-mode)
    (indent-according-to-mode)))

(defun essmeq--paren-expand ()
  "With point on magic space, expand region over following balanced expressions.
This can be followed with `essmeq--paren-slurp' (C-;) to move
those expressions inside the parentheses."
  (interactive)
  (save-excursion
    (ess-up-list)
    (mark-sexp nil t)))

(defun essmeq--paren-slurp (&optional save-initial-space)
  "Moves marked region following paren pair inside parentheses.
Initial spaces following the end of the current paren pair are
deleted unless a prefix argument is given (SAVE-INITIAL-SPACE
non-nil). This is usually preceded by `essmeq--paren-expand' but
applies to any region from point forward."
  (interactive "P")
  (when (and mark-active (> (mark) (point)))
    (let* ((end-of-list (save-excursion (ess-up-list) (point)))
           (delta-ws (if save-initial-space
                         0
                       (save-excursion
                         (ess-up-list)
                         (skip-syntax-forward " "))))
           (end-of-slurp (- (mark) 2 delta-ws))
           (yank-excluded-properties (remq 'keymap yank-excluded-properties)))
      (unless save-initial-space
        (delete-region end-of-list (+ end-of-list delta-ws)))
      (kill-region (point) end-of-list)
      (goto-char end-of-slurp)
      (yank)
      (forward-char -2))))


;;ATTN: magic space killed with = operator. Can it be saved?
(defvar essmeq--paren-map (let ((m (make-sparse-keymap)))
                            (define-key m (kbd ",") 'essmeq--paren-comma)
                            (define-key m (kbd ")") 'essmeq--paren-escape)
                            (define-key m (kbd ";") 'essmeq--paren-expand)
                            (define-key m (kbd "C-;") 'essmeq--paren-slurp)
                            (define-key m [?\t] 'essmeq--paren-escape)
                            m)
  "Keymap active in fresh space in the middle of a new smart open paren.")
(fset 'essmeq--paren-map esseq--paren-map)

(defun ess-smart-equals-open-paren (&optional literal)
  "Inserts properly a properly spaced paren pair with an active keymap inside.
Point is left in the middle of the paren pair and associated with
a special keymap, where tab deletes the extra space and moves
point out of the parentheses and comma inserts a spaced comma,
keeping point on the special space character. "
  (interactive "P")
  (if (or literal (ess-inside-string-p)) ;; pairing in comments seems ok for now
      (self-insert-command (if (integerp literal) literal 1))
    ;; Check syntax table for inferior-ess-r-mode for ', apparently not string
    (let ((skeleton-pair t)
          (skeleton-pair-alist '((?\( _ _ " "
                                      '(let ((pt (point)))
                                         (add-text-properties (1- pt) pt
                                          'essmeq--magic-space t
                                          'keymap 'essmeq--paren-map))
                                      ?\)))))
      (skeleton-pair-insert-maybe nil))))

(defun ess-smart-equals-percent (&option literal)
  "Completion and cycling through %-operators only, unless in comment or string.
Outside a comment or string, this forces a % context as described
in `ess-smart-equals-contexts', so the corresponding list can be
customized to determine ordering. This should be bound to the `%'
key."
    (interactive "P")
  (if literal
      (self-insert-command (if (integerp literal) literal 1))
    (unless (ess-inside-string-or-comment-p)
      (ess-smart-equals-set-overriding-context '%)
      (essmeq--process)
      (unless (eq last-command this-command)
        (setq essmeq--stop-transient
              (set-transient-map essmeq--transient-map
                                 #'essmeq--keep-transient
                                 ess-smart-equals-transient-exit-function))))))


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
    (let* ((mode (if (listp mode-spec) (car mode-spec) mode-spec))
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
  "Insert or complete a properly-spaced R/S (assignment) operator at point.
With a prefix argument (or with LITERAL non-nil) insert this key
literally, repeated LITERAL times if a positive integer.
Otherwise, complete a partial operator or insert a new operator
based on context (major mode and syntactic context) according to
the specification given in `ess-smart-equals-contexts'.
Immediately following invocations of the command cycle through
operators in this context based list in the specified priority
order. Immediately following insertion selected keys (e.g.,
backspace) will remove the inserted operator or (e.g., tab) allow
selection of an inserted operator by completion. See
`ess-smart-equals-cancel-keys'; a shift-modified one of these
keys (except 'C-g') will do a single character deletion and
restore the standard meaning of keys."
  (interactive "P")
  (if (and literal (not (equal literal '(16))))
      (self-insert-command (if (integerp literal) literal 1))
    (when literal
      (message "Cycling over all operators")
      (ess-smart-equals-set-overriding-context 'all))
    (essmeq--process)
    (unless (eq last-command this-command)
      (setq essmeq--stop-transient
            (set-transient-map essmeq--transient-map
                               #'essmeq--keep-transient
                               ess-smart-equals-transient-exit-function))))) 


;;; Minor Mode

;;;###autoload
(define-minor-mode ess-smart-equals-mode
  "Minor mode enabling a smart key for context-aware operator insertion/cycling.

Ess-smart-equals-mode is a buffer-local minor mode. Enabling it
binds a key ('=' by default) to a function that inserts,
completes, or cycles among operators chosen by the syntactic
context at point. These contexts and the priorities of insertion
and cycling are customizable. The operators inserted are usually
assignment operators but can include others as well, e.g.,
comparison operators in and `if' or `while'. When
`ess-smart-equals-extra-ops' is appropriately set, this minor
mode also activates additional smart operators for convenience.

When called interactively, `ess-smart-equals-mode' toggles the
mode without a prefix argument; disables the mode if the prefix
argument is a non-positive integer; and enables the mode if the
prefix argument is a positive integer. When called from Lisp, the
command toggles the mode with argument `toggle'; disables the
mode for a non-positive integer; and enables the mode otherwise,
even with an omitted or nil argument.

Do not set the variable `ess-smart-equals-mode' directly; use the
function of the same name instead."
  :lighter nil
  :keymap ess-smart-equals-mode-map
  (when ess-smart-equals-mode
    (emacs-smart-equals-update-keymaps)
    (ess-smart-equals-set-contexts major-mode)))


(provide 'ess-smart-equals)

;;; ess-smart-equals.el ends here
