Feature: Optimize Use

  Scenario: A fully qualified namespace should be optimized
    When I open temp file "optimize-use"
    And I insert:
    """
    <?php

    namespace Testing;


    class ExtractAMethod
    {
        public function internalMethod()
        {
            $localVariable = new \Top\Level\Domain\Name();
            return $localVariable;
        }
    }
    """
    And I turn on php-mode
    And I turn on php-refactor-mode
    And I press "C-c r ou"
    Then I should see "use Top\Level\Domain\Name"
    And I should see "$localVariable = new Name();"
    And I should not see "new \Top\Level\Domain\Name();"

  Scenario: Optimizing a use statement is an undoable operation
    When I open temp file "optimize-use"
    And I insert:
    """
    <?php

    namespace Testing;


    class ExtractAMethod
    {
        public function internalMethod()
        {
            $localVariable = new \Top\Level\Domain\Name();
            return $localVariable;
        }
    }
    """
    And I turn on php-mode
    And I turn on php-refactor-mode
    And I press "C-c r ou"
    And I press "C-_"
    Then I should not see "use Top\Level\Domain\Name"
    And I should see "new \Top\Level\Domain\Name();"
