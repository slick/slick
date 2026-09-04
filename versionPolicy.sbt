ThisBuild / versionPolicyIntention := Versioning.BumpMajor

ThisBuild / versionPolicyIgnoredInternalDependencyVersions := Some("^\\d+\\.\\d+\\.\\d+-pre\\.\\d+\\.\\w+".r)

ThisBuild / versionPolicyPreviousVersions := CompatReportPlugin.previousRelease.value.toSeq

// main is now developing Slick 4. Compatibility reports against 3.x releases are meaningless, since 4.0.0 is
// intentionally breaking. Only 4.x releases are considered as the previous release, and the report is skipped until
// the first one (4.0.0) has been published.
ThisBuild / compatReportMajorVersion := Some(4)
