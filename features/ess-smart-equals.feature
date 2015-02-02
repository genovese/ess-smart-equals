Feature: Use equals in R source code
  In order to conveniently enter R's multi-character assignment
  As an Emacs user
  I want to use equals to do what I mean

  Scenario: Enter equals that should expand to assignment
    Given I turn on ess-smart-equals
    When I insert "foo "
    And I type "=function() {}"
    Then I should see "foo <- function() {}"

  Scenario: Enter equals that should expand to assignment
    Given I turn on ess-smart-equals
    When I insert "foo    "
    And I type "=function() {}"
    Then I should see "foo    <- function() {}"

  Scenario: Enter equals that should not expand for default arguments
    Given I turn on ess-smart-equals
    When I insert "f(x, answer"
    And I type "=42)"
    Then I should see "f(x, answer=42)"

  Scenario: Enter equals that should expand to equality comparison
    Given I turn on ess-smart-equals
    When I insert "a="
    And I type "=42"
    Then I should see "a == 42"

  Scenario: Enter equals that should expand to inequality comparison
    Given I turn on ess-smart-equals
    When I insert "a!"
    And I type "=42"
    Then I should see "a != 42"

  Scenario: Enter equals that should expand to less-than-or-equals comparison
    Given I turn on ess-smart-equals
    When I insert "a<"
    And I type "=42"
    Then I should see "a <= 42"

  Scenario: Enter equals that should expand to greater-than-or-equals comparison
    Given I turn on ess-smart-equals
    When I insert "a>"
    And I type "=42"
    Then I should see "a >= 42"

  Scenario: Enter equals that should not expand in a comment
    Given I turn on ess-smart-equals
    When I insert "# foo "
    And I type "=function() {}"
    Then I should see "# foo =function() {}"

  Scenario: Enter equals that should not expand in a string
    Given I turn on ess-smart-equals
    When I type '"foo '
    And I type '=function() {}"'
    Then I should see ""foo =function() {}""

  Scenario: Enter underscore that should not expand
    Given I turn on ess-smart-equals
    When I insert "foo"
    And I type "_bar"
    Then I should see "foo_bar"

