Feature: PHP Refactor

  Scenario: Convert Local to Instance Variable
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

  Scenario: Rename Local Variable
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


  Scenario: Extract a Method
    When I open temp file "extract-a-method"
    And I insert:
    """
    <?php

    class ExtractAMethod
    {
        public function internalMethod()
        {
            $localVariable = "Hello";
            $localVariable += " World";

            echo $localVariable;
        }
    }
    """
    And I save the buffer
    And I turn on php-refactor-mode
    And I go to word "$localVariable"
    And I set the mark
    And I go to word "World"
    And I start an action chain
    And I press "C-c r em"
    And I type "extractedMethodName"
    And I execute the action chain
    Then I should see:
    """
    <?php

    class ExtractAMethod
    {
        public function internalMethod()
        {
            $localVariable = $this->extractedMethodName();

            echo $localVariable;
        }

        private function extractedMethodName()
        {
            $localVariable = "Hello";
            $localVariable += " World";

            return $localVariable;
        }
    }
    """
