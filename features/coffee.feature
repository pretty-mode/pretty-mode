Feature: pretty coffee-mode

  Background:
    Given I switch to buffer "*new*"
    And I clear the buffer
    And I turn on coffee-mode
    Then the variable major-mode should be coffee-mode
    And I turn on pretty-mode
    Then the minor mode pretty-mode should be active

  Scenario: ==
    When I insert:
      """
      assert 1 == 1
      assert 1==1
      """
    And I run redisplay
    Then the buffer should appear as:
      """
      assert 1 ⩵ 1
      assert 1⩵1
      """
