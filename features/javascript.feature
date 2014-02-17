Feature: pretty javascript-mode

  Background:
    Given I switch to buffer "*new*"
    And I clear the buffer
    And I turn on js2-mode
    And I turn on pretty-mode

  Scenario: modes
    Then the variable major-mode should be js2-mode
    And the minor mode pretty-mode should be active

  Scenario: ==
    When I insert:
      """
      1 == 1
      
      """
    And I run redisplay
    Then the buffer should appear as:
      """
      1 ⩵ 1
      
      """
