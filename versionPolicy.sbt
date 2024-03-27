ThisBuild / versionPolicyIntention := Versioning.BumpMinor

ThisBuild / versionPolicyIgnoredInternalDependencyVersions := Some("^\\d+\\.\\d+\\.\\d+-pre\\.\\d+\\.\\w+\\.dirty".r)

ThisBuild / versionPolicyPreviousVersions := CompatReportPlugin.previousRelease.value.toSeq
