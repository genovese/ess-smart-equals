
# ess-smart-equals.el

> Current Version: 0.3.0. This is a major update &#x2013; indeed a
> complete rewrite &#x2013; relative to version 0.2.2, with significant
> improvements in functionality, flexibility, and performance.
> This has also been brought up to date with recent versions of
> ESS.

Assignment in R is syntactically complicated by a few features:

1.  the historical role of '\_' (underscore) as an assignment

character in the S language; 2. the somewhat
inconvenient-to-type, if conceptually pure, '<-' operator as the
preferred assignment operator; 3. the ability to use either an
'=', '<-', and a variety of other operators for assignment; and

1.  the multiple roles that '=' can play, including for setting

named arguments in a function call.

This package offers a flexible, context-sensitive assignment key
for R and S that is, by default, tied to the '=' key. This key
inserts or completes relevant, properly spaced operators
(assignment, comparison, etc.) based on the syntactic context in
the code. It allows very easy cycling through the possible
operators in that context. The contexts, the operators, and
their cycling order in each context are customizable.

The package defines a buffer-local minor mode
`ess-smart-equals-mode`, intended for S-language modes (e.g.,
ess-r-mode, inferior-ess-r-mode, and ess-r-transcript-mode), that
when enabled in a buffer activates the '=' key to to handle
context-sensitive completion and cycling of relevant operators. When
the mode is active and an '=' is pressed:

1.  With a prefix argument or in specified contexts (which for
    most major modes means in strings or comments), just
    insert '='.

2.  If an operator relevant to the context lies before point
    (with optional whitespace), it is replaced, cyclically, by the
    next operator in the configured list for that context.

3.  Otherwise, if a prefix of an operator relevant to the
    context lies before point, that operator is completed.

4.  Otherwise, the highest priority relevant operator is inserted
    with surrounding whitespace (see `ess-smart-equals-no-spaces`).

Consecutive presses of '=' cycle through the relevant operators.
After an '=', a backspace (or other configurable keys) removes
the last operator and tab offers a choice of operators by completion.
(Shift-backspace will delete one character only and restore the
usual maning of backspace.) See `ess-smart-equals-cancel-keys`.

By default, the minor mode activates the '=' key, but this can
be customized by setting the option `ess-smart-equals-key` before
this package is loaded.

The function `ess-smart-equals-activate` arranges for the minor mode
to be activated by mode hooks for any given list of major modes,
defaulting to ESS major modes associated with R (`ess-r-mode`,
`inferior-ess-r-mode`, `ess-r-transcript-mode`, `ess-roxy-mode`). 


## Examples

In the left column below, ^ marks the location at which an '='
key is pressed, the remaining columns show the result of
consecutive presses of '=' using the package's default settings.
position of point.

    Before '='         Press '='      Another '='       Another '='
    ----------         ---------      -----------       -----------
    foo^               foo <- ^       foo <<- ^         foo = ^
    foo  ^             foo  <- ^      foo  <<- ^        foo  = ^
    foo<^              foo <- ^       foo <<- ^         foo = ^
    foo=^              foo = ^        foo -> ^          foo ->> ^
    foo(a^             foo(a = ^      foo( a == ^       foo( a != ^
    if( foo=^          if( foo == ^   if( foo != ^      if( foo <= ^
    if( foo<^          if( foo < ^    if( foo > ^       if( foo >= ^
    "foo ^             "foo =^        "foo ==^          "foo ===^
    #...foo ^          #...foo =^     #...foo ==^       #...foo ===^

As a bonus, the value of the variable `ess-smart-equals-extra-ops`
when this package is loaded, determines some other smart operators
that may prove useful. Currently, only `brace`, `paren`, and `percent`
are supported, causing `ess-smart-equals-open-brace`,
`ess-smart-equals-open-paren`, and `ess-smart-equals-percent` to be
bound to '{', '(', and '%', respectively. The first two of these
configurably places a properly indented and spaced matching pair at
point or around the region if active. The paren pair also includes
a magic space with a convenient keymap for managing parens. See the
readme. See the customizable variable
`ess-smart-equals-brace-newlines` for configuring the newlines around
braces. The third operator (`ess-smart-equals-percent`) performs
matching of %-operators analogously to `ess-smart-equals`. See the
**Extra Operators** section below under **Customization** for details.

Finally, the primary user facing functions are named with a
prefix `ess-smart-equals-` to avoid conflicts with other
packages. Because this is long, the internal functions and
objects use a shorter (but still distinctive) prefix `essmeq-`.


## Installation and Initialization

The package can be loaded from MELPA using `package-install` with

    M-x package-install ess-smart-equals

or with the `list-packages` interface or another Emacs package
manager. Alternatively, you can clone or download the source
directly from the github repository and put the file
`ess-smart-equals.el` in your Emacs load path.

To activate, you need only do

    (with-eval-after-load 'ess-r-mode
      (require 'ess-smart-equals)
      (ess-smart-equals-activate))

somewhere in your init file. This will add `ess-smart-equals-mode` to 
a prespecified, but customizable, list of mode hooks and activate
the mode in already active buffers.

For those who use the outstanding `use-package`, you can do

    (use-package ess-smart-equals
      :after (:any ess-r-mode inferior-ess-r-mode ess-r-transcript-mode)
      :config (ess-smart-equals-activate))

somewhere in your init file. An equivalent but less concise version
of this is

    (use-package ess-smart-equals
      :after (:any ess-r-mode inferior-ess-r-mode ess-r-transcript-mode)
      :hook ((ess-r-mode . ess-smart-equals-mode)
             (inferior-ess-r-mode . ess-smart-equals-mode)
             (ess-r-transcript-mode . ess-smart-equals-mode)
             (ess-roxy-mode . ess-smart-equals-mode))

To also activate the extra smart operators and automatically bind
them, you can replace this with

    (use-package ess-smart-equals
      :init   (setq ess-smart-equals-extra-ops '(brace paren percent))
      :after  (:any ess-r-mode inferior-ess-r-mode ess-r-transcript-mode)
      :config (ess-smart-equals-activate))

You can also enable the minor mode in any buffer with

    M-x ess-smart-equals-mode

though you will typically want to enable the mode
in a mode hook, e.g.,

    (add-hook 'foo-mode-hook 'ess-smart-equals-mode)

to enable the mode in `foo-mode` buffers.


## Customization


### Special Keys

By default `ess-smart-equals-mode` binds the smart operator to the '='
key, and this is the recommended choice. However, this key can be
changed by customizing the variable `ess-smart-equals-key`. This
should be changed either with the customization facility or before
the package is loaded, as the key affects several internal keymaps.

When `ess-smart-equals-key` is pressed, several transient keys are
bound. First, the basic key of `ess-smart-equals-key` (e.g., '='
for '=' or 'C-c ='.) reexecutes `ess-smart-equals`, cycling the
operators according to context. Any other key exits the transient
keymap. Second, any key in `ess-smart-equals-cancel-keys` deletes any
inserted operator before point; a shifted version of such a
key (except `C-g`) deletes a single character backwards and thus
cancels the transient bindings. Finally, tab allows you to select an
operator by completion.

An advanced customization is to change the condition for exiting the
transient map in this situation. See
`ess-smart-equals-transient-exit-function`.

Several other useful smart operators can be configured;
see **Extra Operators** below.


### Contexts

The operator inserted or completed by `ess-smart-equals` is determined
by the major mode and the syntactic context at point. The customizable
variable `ess-smart-equals-contexts` specifies the mapping
from syntactic contexts to a list of operators to consider in the
order specified. This mapping is given for all contexts for the
default case (t) along with lists for any major mode 
that are merged into the default mapping under that mode.
In this way, simple modifications can be applied to any relevant
mode without repeating all the specifications.

The user can create new contexts by adding additional keys to that
mapping and defining `ess-smart-equals-context-function`. This is
called first when the context is determined; if it returns a symbol,
that is used as the context; if it returns nil, the built-in context
calculation is performed. An advanced customization allows local
change to the context, see `ess-smart-equals-overriding-context`.


### Hooks

The hook `ess-smart-equals-mode-hook` is called whenever the minor
mode is enabled or disabled. The hooks `ess-smart-equals-mode-on-hook`
and `ess-smart-equals-mode-off-hook` are called when the mode is
enabled or disabled, respectively.

The variable `ess-smart-equals-narrow-function` is used to narrow
the buffer to a specific region where the ESS syntax checking will
be valid. This is used primarily in `inferior-ess-r-mode` to restrict
attention to the current prompt line or the zone between prompts
because output or erroneous commands can adversely effect the
ESS syntax checking. This should not normally be needed otherwise,
but it can be set in `ess-smart-equals-options` if desired.

The customizable variable `ess-smart-equals-insertion-hook`, if set,
allows arbitrary post-processing after an operator insertion. It is
passed all the information needed to characterize the insertion; see
the documentation for that variable for details. 


### Extra Operators

If `ess-smart-equals-extra-ops` is non-nil, it should be a list
containing some of the symbols `brace`, `paren`, or `percent`.
These settings will cause '{', '(', and '%', respectively, to
be bound in the minor mode map to a smart operator with
the following features:

-   `brace`
    
    Binds '{' to the command `ess-smart-equals-open-brace`. This
    inserts a properly spaced and indented pair of braces, wrapping
    around the region if it is active. The customizable variable
    `ess-smart-equals-brace-newlines` controls the placement of
    newlines before and after each brace. This can be configured
    separately or added to your ESS style as desired.

-   `paren`
    
    Binds '(' to the command `ess-smart-equals-open-paren`. This
    inserts a matching pair of parentheses with point on a magic
    space between them. If region is active, it is wrapped by the
    parentheses. The magic space has an attached keymap
    that makes it easy to fill, escape, and expand the parenthesis
    pair. In particular, when on this space:
    
    -   ')' or TAB eliminates the magic space and exits the parentheses;
    
    -   ',' inserts a spaced comma, leaving point on the magic space;
    
    -   ';' expands the region after the parenthesis pair to encompass
        an additional balanced expression; and
    
    -   'C-;' moves the marked region after the pair (e.g., as constructed
        by ';') inside the parentheses, eliminating leading spaces unless
        a prefix argument is given.
    
    Taken together, these make it fast to fill in function calls
    or conditionals.

-   `percent`
    
    Binds '%' to the command `ess-smart-equals-percent`. This
    provides expansion, cycling, and completion analogous to
    `ess-smart-equals` but for %-operators in R.

With a prefix argument, all of these insert the literal corresponding
character, with repeats if the argument is numeric.

Additional smart operators may be added in future versions.    


## Change Log

-   **0.3.0:** Breaking changes in functionality, design, and configuration.
    No longer relies on `ess-S-assign` which was deprecated in
    ESS. Now provides more powerful context-sensitive, prioritized
    operator lists with cycling and completion. The mode is now,
    properly, a local minor mode, which can be added automatically
    to relevant mode hooks for ESS R modes. Updated required
    versions of emacs and ESS.

-   **0.2.2:** ATTN:Fix for deprecated ESS variables `ess-S-assign` and
    `ess-smart-S-assign-key`.

-   **0.2.1:** Initial release with simple insertion and completion, with
    space padding for the operators except for a single '=' 
    used to specify named arguments in function calls. Relies on
    ESS variables `ess-S-assign` and `ess-smart-S-assign-key`
    to specify preferred operator for standard assignments.


## To Do

-   Handle narrowing in inferior mode
-   More flexible contexts
-   Sticky context during cycling

