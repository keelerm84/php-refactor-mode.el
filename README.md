# php-refactor-mode

This is a minor mode which provides convenient access to the refactoring
methods provided by
[php-refactoring-browser](https://github.com/QafooLabs/php-refactoring-browser).

## Usage

You must require the minor-mode and then enable it.

    (require 'php-refactor-mode)
    (php-refactor-mode)

Options either operate on the symbol at point, the selected region, or the file
as a whole.  Each operation can be triggered through the use of a particular
keybinding.

### Keybindings

All keybindings in php-refactor-mode start with `C-c r` followed by some
mnemonic shortcut.

* `lv`: Convert a local variable to an instance variable
* `rv`: Rename a local variable
* `em`: Select a region and extract it to a new method
* `ou`: Optimize use statements for FQCNs

## Development

To fetch the test dependencies, install
[cask](https://github.com/rejeep/cask.el) if you haven't already, then:

    $ cd /path/to/php-refactor-mode
    $ cask

Run the tests with:

    $ cask exec ecukes

Or alternatively, if you have [watchr](https://github.com/mynyml/watchr)
installed, you can run continuously run the tests with:

    $ watchr watch-tests.watchr
