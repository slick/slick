ThisBuild / versionPolicyIntention := Versioning.BumpMajor

ThisBuild / versionPolicyIgnoredInternalDependencyVersions := Some("^\\d+\\.\\d+\\.\\d+-pre\\.\\d+\\.\\w+".r)

ThisBuild / versionPolicyPreviousVersions := CompatReportPlugin.previousRelease.value.toSeq

// Major version under development. Until a tag of this major version is reachable, untagged commits are versioned as
// <major>.0.0-pre.<n>.<sha>. The compatibility report only compares against releases of the same major version; while
// none has been published yet, the report is skipped. Bump when work on the next major version starts.
ThisBuild / developmentMajorVersion := Some(4)
