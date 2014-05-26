Feature: Convert Local to Instance Variable

  Scenario: A local variable should be converted to an instance variable
    When I open temp file "convert-local-to-instance-variable"
    And I insert:
    """
    <?php

    class ConvertLocalToInstanceVariable
    {
        public function internalMethod()
        {
            $localVariable = "This needs to be an instance variable";

            echo $localVariable;
        }
    }
    """
    And I save the buffer
    And I turn on php-refactor-mode
    And I go to word "$localVariable"
    And I press "C-c r lv"
    Then I should see "private $localVariable"
    And I should not see pattern "^ *\\$localVariable"
