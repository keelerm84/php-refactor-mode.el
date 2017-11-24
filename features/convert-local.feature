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
    And I turn on php-mode
    And I turn on php-refactor-mode
    And I place the cursor before "$localVariable"
    And I press "C-c r lv"
    Then I should see "private $localVariable"
    And I should not see pattern "^ *$localVariable"

  Scenario: A local variable should be converted to an instance variable when point is in middle of variable
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
    And I turn on php-mode
    And I turn on php-refactor-mode
    And I place the cursor after "$localV"
    And I press "C-c r lv"
    Then I should see "private $localVariable"
    And I should not see pattern "^ *$localVariable"

  Scenario: A local variable should be converted to an instance variable when using snake case
    When I open temp file "convert-local-to-instance-variable"
    And I insert:
    """
    <?php

    class ConvertLocalToInstanceVariable
    {
        public function internalMethod()
        {
            $local_variable = "This needs to be an instance variable";

            echo $local_variable;
        }
    }
    """
    And I turn on php-mode
    And I turn on php-refactor-mode
    And I place the cursor before "$local_variable"
    And I press "C-c r lv"
    Then I should see "private $local_variable"
    And I should not see pattern "^ *$local_variable"

  Scenario: Converting local variable is an undoable operation
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
    And I turn on php-mode
    And I turn on php-refactor-mode
    And I place the cursor before "$localVariable"
    And I press "C-c r lv"
    And I press "C-_"
    Then I should not see "private $localVariable"
    And I should see pattern "^ *$localVariable"
