# How to contribute

Some Slick-specific clarifications/additions:

- External contributions are generally reviewed by one core developer.
- We have GitHub Actions and Appveyor for continuous integration testing.
- We have the standard Github [issue tracker](https://github.com/slick/slick/issues?state=open) for bug reports. When you're referencing an issue number in a commit, please prefix it with "issue #".
- Branches starting with "tmp/" or "wip/" on the official repo are considered unstable and should not be used as a basis for your work. Their history may be rewritten at any time without notice.

These guidelines are meant to be a living document that should be changed and adapted as needed. We encourage changes that makes it easier to achieve our goals in an efficient way.

## General Workflow

This is the process for committing code into `main`. There are of course exceptions to these rules, for example minor changes to comments and documentation, fixing a broken build etc.

1. Before starting to work on a feature or a fix, you have to make sure that:
    1. There is a ticket for your work in the project's issue tracker. If not, create it first.
    2. The ticket has been scheduled for the current milestone.
    3. The ticket is estimated by the team.
    4. The ticket have been discussed and prioritized by the team.
2. You should always perform your work in a Git feature branch. The branch should be given a descriptive name that explains its intent. Some teams also like adding the ticket number and/or the [GitHub](http://github.com) user ID to the branch name, these details is up to each of the individual teams.
3. When the feature or fix is completed you should open a [Pull Request](https://help.github.com/articles/using-pull-requests) on GitHub.
4. The Pull Request should be reviewed by other maintainers (as many as feasible/practical). Anyone is welcome to participate in the review process; it is not a closed process.
5. After the review you should fix the issues as needed (pushing a new commit for new review etc.), iterating until the reviewers give their thumbs up.
6. Once the code has passed review the Pull Request can be merged into the `main` branch.

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

## Work In Progress

It is ok to work on a public feature branch in the GitHub repository. Something that can sometimes be useful for early feedback etc. If so then it is preferable to name the branch accordingly. This can be done by either prefix the name with ``wip-`` as in ‘Work In Progress’, or use hierarchical names like ``wip/..``, ``feature/..`` or ``topic/..``. Either way is fine as long as it is clear that it is work in progress and not ready for merge. This work can temporarily have a lower standard. However, to be merged into `main` it will have to go through the regular process outlined above, with Pull Request, review etc..

Also, to facilitate both well-formed commits and working together, the ``wip`` and ``feature``/``topic`` identifiers also have special meaning.   Any branch labelled with ``wip`` is considered “git-unstable” and may be rebased and have its history rewritten.   Any branch with ``feature``/``topic`` in the name is considered “stable” enough for others to depend on when a group is working on a feature.

## Creating Commits And Writing Commit Messages

Follow these guidelines when creating public commits and writing commit messages.

1. If your work spans multiple local commits (for example; if you do safe point commits while working in a feature branch or work in a branch for long time doing merges/rebases etc.) then please do not commit it all but rewrite the history by squashing the commits into a single big commit which you write a good commit message for (like discussed in the following sections). For more info read this article: [Git Workflow](http://sandofsky.com/blog/git-workflow.html). Every commit should be able to be used in isolation, cherry picked etc.
2. First line should be a descriptive sentence what the commit is doing. It should be possible to fully understand what the commit does by just reading this single line. It is **not ok** to only list the ticket number, type "minor fix" or similar. Include reference to ticket number, prefixed with #, at the end of the first line. If the commit is a small fix, then you are done. If not, go to 3.
3. Following the single line description should be a blank line followed by an enumerated list with the details of the commit.
4. Add keywords for your commit (depending on the degree of automation we reach, the list may change over time):
    * ``Review by @gituser`` - if you want to notify someone on the team. The others can, and are encouraged to participate.
    * ``Fix/Fixing/Fixes/Close/Closing/Refs #ticket`` - if you want to mark the ticket as fixed in the issue tracker.
    * ``backport to _branch name_`` - if the fix needs to be cherry-picked to another branch (like 2.9.x, 2.10.x, etc)

Example:

    Adding monadic API to Future. Fixes #2731

      * Details 1
      * Details 2
      * Details 3
