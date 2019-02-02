Feature: Use smart operators in R source code
  In order to conveniently enter paired parentheses in R
  As an Emacs user
  I want to use parentheses to do what I mean

  Background: 
    Given I turn on ess-smart-equals

  #
  # Parenthesis
  #

  Scenario: Smart parentheses activated
    Then key "(" should be bound in minor mode map

  Scenario: Use parentheses, end on magic space, see paired parens
    When I insert "foo"
    And I type "("
    Then current point should be on a magic space with character " "
    And I should see "foo( )"

  Scenario: Use parentheses, escape parens, see paired parens
    When I insert "foo"
    And I type "()"
    Then I should see "foo()"

  Scenario: Use parentheses, add args, escape parens, see paired parens
    When I insert "foo"
    And I type "(a=2,b=4,c=8)"
    Then I should see "foo(a = 2, b = 4, c = 8)"

  Scenario: Use parentheses to capture additional arguments
    When I insert "foo"
    And I set the mark
    And I insert " 8"
    And I pop the mark
    And I type "(a=2,b=4,c="
    And I press "C-;"
    And I press "M-;"
    And I press ";"
    Then I should see "foo(a = 2, b = 4, c = 8)"

  Scenario: Smart parens do not work in comment
    When I insert "# abc"
    And I type "("
    And I insert "u"
    And I go to end of line
    And I insert "v"
    Then I should see "# abc(uv"

  Scenario: Smart parens do not work in string
    When I insert "" abc"
    And I type "("
    And I insert "u"
    And I go to end of line
    And I insert "v"
    Then I should see "" abc(uv"

  #
  # Percent
  #

  Scenario: Smart percent activated
    Then key "%" should be bound in minor mode map

  Scenario: Insert percent operator
    When I insert "a"
    And I type "%b"
    Then I should see "a %*% b"

  Scenario: Insert percent operator, cycle once
    When I insert "a"
    And I type "%%b"
    Then I should see "a %% b"

  Scenario: Insert percent operator, cycle twice
    When I insert "a"
    And I type "%%%b"
    Then I should see "a %/% b"

  Scenario: Insert percent operator, cycle thrice
    When I insert "a"
    And I type "%%%%b"
    Then I should see "a %in% b"

  Scenario: Complete percent operator
    When I insert "a %i"
    And I type "%b"
    Then I should see "a %in% b"

  Scenario: Cycling with equals and percent interleaved 112211
    When I insert "a"
    And I type "==%%==b"
    Then I should see "a <<- b"

  Scenario: Cycling with equals and percent interleaved 11222
    When I insert "a"
    And I type "==%%%b"
    Then I should see "a %/% b"

  Scenario: Cycling with equals and percent interleaved 222111
    When I insert "a"
    And I type "%%%===b"
    Then I should see "a = b"

  Scenario: Cycling with equals and percent interleaved 112211, conditional
    When I insert "if"
    And I type "(a==%%%%==b;"
    Then I should see "if(a != b)"

  Scenario: Cycling with equals and percent interleaved 22221111, conditional
    When I insert "if"
    And I type "(a%%%%====b;"
    Then I should see "if(a <= b)"


  #
  # Braces
  #

  Scenario: Smart braces activated
    Then key "{" should be bound in minor mode map

  Scenario: Use braces to delimit a block with point at first line
    When I insert "if( a < b )"
    And I inhibit messages
    And I type "{"
    And I allow messages
    Then the cursor should be at point "19"

  Scenario: Use braces to delimit a block
    When I insert "if( a < b )"
    And I inhibit messages
    And I type "{"
    And I allow messages
    Then I should see across lines
    """
    if( a < b ) {
        
    }
    """

  Scenario: Smart braces do not work in comment
    When I insert "# abc"
    And I type "{"
    And I inhibit messages
    And I go to end of buffer
    And I allow messages
    And I insert "u"
    Then I should see "# abc{u"

  Scenario: Smart braces do not work in string
    When I insert "" abc"
    And I type "{"
    And I inhibit messages
    And I go to end of buffer
    And I allow messages
    And I insert "u"
    Then I should see "" abc{u"
