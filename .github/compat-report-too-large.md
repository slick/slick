## Incompatible changes

The compatibility report is too large to post as a PR comment —
this typically means a very large number of API changes relative
to the previous release.

The full report is available as a downloadable artifact from the
workflow run.

To generate it locally, run:

```
sbt site/buildCompatReport
```

The report will be written to `site/target/compat-report.md`.
