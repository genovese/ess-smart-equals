Feature: Use equals in R source code
  In order to conveniently enter R's multi-character assignment
  As an Emacs user
  I want to use equals to do what I mean

  Background: 
    Given I turn on ess-smart-equals

  Scenario: Enter equals that should expand to assignment
    When I insert "foo "
    And I type "=function"
    Then I should see "foo <- function"

  Scenario: Enter multiple equals that should expand to parent assignment operator
    When I insert "foo"
    And I repeat "=" 2 times
    And I type "a"
    Then I should see "foo <<- a"

  Scenario: Enter multiple equals that should expand to equals operator
    When I insert "foo"
    And I repeat "=" 3 times
    And I type "a"
    Then I should see "foo = a"

  Scenario: Enter multiple equals that should expand to assignment operator
    When I insert "foo"
    And I repeat "=" 8 times
    And I type "a"
    Then I should see "foo <- a"

  Scenario: Enter equals in call that should not expand for default arguments
    When I insert "f(x, answer"
    And I type "=42)"
    Then I should see "f(x, answer = 42)"

  Scenario: Enter equals in conditional that should expand to equality comparison
    When I type "if( a="
    And I type "42 )"
    Then I should see "if( a == 42 )"

  Scenario: Enter equals several times that should cyclically expand to neq comparison
    When I type "if( a"
    And I repeat "=" 2 times
    And I type "42 )"
    Then I should see "if( a != 42 )"

  Scenario: Enter equals several times that should cyclically expand to leq comparison
    When I type "if( a"
    And I repeat "=" 3 times
    And I type "42 )"
    Then I should see "if( a < 42 )"

  Scenario: Enter equals several times that should cyclically expand to geq comparison
    When I type "if( a"
    And I repeat "=" 6 times
    And I type "42 )"
    Then I should see "if( a >= 42 )"

  Scenario: Enter equals several times that should cyclically expand to equality comparison
    When I type "if( a"
    And I repeat "=" 8 times
    And I type "42 )"
    Then I should see "if( a == 42 )"

  Scenario: Enter equals that should complete an inequality comparison
    When I type "if( a<="
    And I type "42 )"
    Then I should see "if( a <= 42 )"

  Scenario: Enter equals that should complete to inequality comparison
    When I insert "while( a!"
    And I type "=42 )"
    Then I should see "while( a != 42 )"

  Scenario: Enter equals that should complete to leq comparison
    When I insert "while( a<"
    And I type "=42 )"
    Then I should see "while( a <= 42 )"

  Scenario: Enter equals that should complete to geq comparison
    When I insert "while( a>"
    And I type "=42 )"
    Then I should see "while( a >= 42 )"

  Scenario: Enter equals in index that should complete to eq comparison
    When I insert "a[u"
    And I type "=42,]"
    Then I should see "a[u == 42,]"

  Scenario: Enter equals in index that should complete to .in. comparison
    When I insert "a[u"
    And I repeat "=" 3 times
    And I type "v, ]"
    Then I should see "a[u %in% v, ]"

  Scenario: Enter equals that should not expand in a comment
    When I insert "# foo "
    And I type "= function"
    Then I should see "# foo = function"

  Scenario: Enter equals that should not expand in a string
    When I enter '"foo '
    And I enter '= function"'
    Then I should see ""foo = function""

  Scenario: Enter underscore that should not expand
    When I insert "foo"
    And I type "_bar"
    Then I should see "foo_bar"

  Scenario: Use backspace to remove an assignment operator
    When I insert "foo"
    And I type "="
    And I press "<backspace>"
    Then I should see "foo"

  Scenario: Use backspace to remove an assignment operator after cycle
    When I insert "foo"
    And I type "=="
    And I press "<backspace>"
    Then I should see "foo"

  Scenario: Use backspace to remove an assignment operator after two cycles
    When I insert "foo"
    And I type "==="
    And I press "<backspace>"
    Then I should see "foo"
