Feature: Rename Local Variable

  Scenario: A variable should be renamed
    When I open temp file "rename-local-variable"
    And I insert:
    """
    <?php

    class RenameLocalVariable
    {
        public function internalMethod()
        {
            $localVariable = "This variable needs renamed.";

            echo $localVariable;
        }
    }
    """
    And I save the buffer
    And I turn on php-refactor-mode
    And I go to word "$localVariable"
    And I start an action chain
    And I press "C-c r rv"
    And I type "renamedVariable"
    And I execute the action chain
    And I should not see "$localVariable"
    And I should see "$renamedVariable"
