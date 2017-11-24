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
    And I turn on php-mode
    And I turn on php-refactor-mode
    And I place the cursor before "$localVariable"
    And I start an action chain
    And I press "C-c r rv"
    And I type "renamedVariable"
    And I execute the action chain
    Then I should not see "$localVariable"
    And I should see "$renamedVariable"

  Scenario: A variable should be renamed when point is in middle of variable
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
    And I turn on php-mode
    And I turn on php-refactor-mode
    And I place the cursor after "$localV"
    And I start an action chain
    And I press "C-c r rv"
    And I type "renamedVariable"
    And I execute the action chain
    Then I should not see "$localVariable"
    And I should see "$renamedVariable"

  Scenario: A variable should be renamed when using snake case
    When I open temp file "rename-local-variable"
    And I insert:
    """
    <?php

    class RenameLocalVariable
    {
        public function internalMethod()
        {
            $local_variable = "This variable needs renamed.";

            echo $local_variable;
        }
    }
    """
    And I turn on php-mode
    And I turn on php-refactor-mode
    And I place the cursor before "$local_variable"
    And I start an action chain
    And I press "C-c r rv"
    And I type "renamed_variable"
    And I execute the action chain
    Then I should not see "$local_variable"
    And I should see "$renamed_variable"

  Scenario: Renaming a variable is an undoable operation
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
    And I turn on php-mode
    And I turn on php-refactor-mode
    And I place the cursor before "$localVariable"
    And I start an action chain
    And I press "C-c r rv"
    And I type "renamedVariable"
    And I execute the action chain
    And I press "C-_"
    Then I should see "$localVariable"
    And I should not see "$renamedVariable"
