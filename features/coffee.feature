Feature: pretty coffee-mode

  Background:
    Given I switch to buffer "*new*"
    And I clear the buffer
    And I turn on coffee-mode
    And I turn on pretty-mode

  Scenario: modes
    Then the variable major-mode should be coffee-mode
    And the minor mode pretty-mode should be active

  Scenario: ==
    When I insert:
      """
      a == 15
      
      """
    And I run redisplay
    Then the buffer should appear as:
      """
      a ⩵ 15
      
      """
