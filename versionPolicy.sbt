ThisBuild / versionPolicyIntention := Versioning.BumpMajor

ThisBuild / versionPolicyIgnoredInternalDependencyVersions := Some("^\\d+\\.\\d+\\.\\d+-pre\\.\\d+\\.\\w+".r)

ThisBuild / versionPolicyPreviousVersions := CompatReportPlugin.previousRelease.value.toSeq

// Major version series under development. The compatibility report only compares against releases of this series;
// while none has been published yet, the report is skipped. Bump when work on the next major version starts.
ThisBuild / compatReportMajorVersion := Some(4)
