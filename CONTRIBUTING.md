# How to contribute

Some Slick-specific clarifications/additions:

- Base your work on `main`.

These guidelines are meant to be a living document that should be changed and adapted as needed. We encourage changes that makes it easier to achieve our goals in an efficient way.

## General Workflow

This is the process for committing code into `main`.

1. When the feature or fix is completed you should open a [Pull Request](https://help.github.com/articles/using-pull-requests) on GitHub.
2. The Pull Request should be reviewed by other maintainers. Anyone is welcome to participate in the review process; it is not a closed process.
3. After the review you should fix the issues as needed (pushing a new commit for new review etc.), iterating until the reviewers give their thumbs up.
4. Once the code has passed review the Pull Request can be merged into the `main` branch.

## Pull Request Requirements

For a Pull Request to be considered at all it has to meet these requirements:

1. Live up to the current code standard:
   - Not violate [DRY](http://web.archive.org/web/20180511202436/http://programmer.97things.oreilly.com/wiki/index.php/Don't_Repeat_Yourself).
   - [Boy Scout Rule](http://web.archive.org/web/20180523180601/http://programmer.97things.oreilly.com/wiki/index.php/The_Boy_Scout_Rule) needs to have been applied.
2. Regardless if the code introduces new features or fixes bugs or regressions, it must have comprehensive tests.
3. The code must be well documented (see the ‘Documentation’ section below).

If these requirements are not met then the code should **not** be merged into `main`, or even reviewed - regardless of how good or important it is. No exceptions.

## Documentation

Example code in the documentation should be run as part of an automated test suite.

## External Dependencies

New runtime dependencies must have a permissive license that is compatible with Slick's BSD license.

## Creating Commits And Writing Commit Messages

Follow these guidelines when creating public commits and writing commit messages.

1. Every commit should be able to be used in isolation, cherry picked etc.
2. First line should be a descriptive sentence what the commit is doing. It should be possible to fully understand what the commit does by just reading this single line. It is **not ok** to only list the ticket number, type "minor fix" or similar.
3. If there is more to say, follow the first line with a blank line and the details, in whatever form fits.
4. Add keywords for your commit (depending on the degree of automation we reach, the list may change over time):
    * ``Fix/Fixing/Fixes/Close/Closing/Refs #ticket`` - if you want to mark the ticket as fixed in the issue tracker.
