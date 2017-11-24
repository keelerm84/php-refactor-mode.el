Feature: Extract a Method

  Scenario: A region should be extracted to a new method
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
    And I turn on php-mode
    And I turn on php-refactor-mode
    And I place the cursor before "$localVariable"
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

  Scenario: Extracting a method is an undoable operation
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
    And I turn on php-mode
    And I turn on php-refactor-mode
    And I place the cursor before "$localVariable"
    And I set the mark
    And I go to word "World"
    And I start an action chain
    And I press "C-c r em"
    And I type "extractedMethodName"
    And I execute the action chain
    And I press "C-_"
    Then I should see:
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
