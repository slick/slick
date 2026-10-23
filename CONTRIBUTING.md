# How to contribute

These guidelines are meant to be a living document that should be changed and adapted as needed. We encourage changes
that make it easier to achieve our goals in an efficient way.

## Pull requests

- Base your work on `main`.
- Open your pull request as a draft until it is ready. Once it is approved and CI passes, it is merged automatically.
- If your pull request fixes an issue, put `Fixes #N` in its description, so that GitHub closes the issue when the pull
  request is merged.
- When you address review comments, fold each fix into the commit it belongs to and force-push.
- Anyone is welcome to review pull requests.

A pull request is merged only once it meets these requirements:

- **Tests.** Changes in behavior need tests. For a new feature, write a test for each thing you are afraid could go
  wrong. For a bug fix, add a test that fails without the fix and passes with it. [DEVELOPING.md](DEVELOPING.md)
  explains how to run the tests.
- **Documentation.** Changes that users can see, in the public API or in behavior, need documentation:
  - Scaladoc for API changes;
  - a section in the manual (`doc/paradox`) for new or changed features;
  - an entry in the migration guide (`doc/paradox`) for changes that break users' code.

  Code examples in the manual live in `doc/code`, where `testkit/DocTest` compiles and runs them, and are included in
  the page with `@@snip`.
- **Don't repeat yourself.** Don't copy and paste code; factor out the duplication. Reuse an existing helper instead
  of writing a new one. Define each fact, such as a version or a type mapping, in one place.
- **Unrelated cleanups in separate commits.** Cleanups to the code around your change are welcome, but put the ones
  that are unrelated to your change in their own commits.
- **Dependencies.** New runtime dependencies must have a permissive license that is compatible with Slick's BSD
  license.

## Commits

- Every commit must compile. A commit that adds a deliberately failing test, which a later commit makes pass, is fine.
- The first line of the commit message says what the commit does, so that it can be understood on its own. An issue
  number alone, or "minor fix", is not enough.
- If there is more to say, follow the first line with a blank line and the details, in whatever form fits.

## Issue labels

Every open issue has one type label:

- `bug`: Slick does something wrong.
- `improvement`: a feature request, an enhancement, or a documentation or refactoring wish.

These flags say how an issue can be worked on:

- `small-fix`: a small, well-scoped change with a clear fix that needs no design decisions. A good place to start,
  for people and for coding agents.
- `needs-retest`: reported on an older Slick and not yet confirmed on the current release. Reproduce it first; if it
  no longer happens, say so in the issue so that it can be closed.
- `needs-major-version`: the change breaks the public API or existing behaviour, so it can only go into a major
  release. If you find a way to do it without breaking anything, say so in the issue.
- `correctness`: Slick silently returns wrong results or loses data. These come first.

The `topic:*` labels give the area: `topic:query-compiler`, `topic:driver` (database-specific profile behaviour),
`topic:codegen`, `topic:plainsql`, `topic:dbio` (actions, execution, transactions, streaming and connection pools),
`topic:documentation`, `topic:usability` (API ergonomics and error messages) and `topic:reactive`.
