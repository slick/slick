import java.io.File
import java.util.Date

import Versioning.VersionInfo.VersionType
import com.github.sbt.git.DefaultReadableGit
import coursier.version.Version
import sbt.Keys.version
import sbt.{AutoPlugin, settingKey}
import sbtdynver.DynVerPlugin.autoImport.{dynver, dynverCurrentDate, dynverGitDescribeOutput}
import sbtdynver.{DynVer, GitDescribeOutput}
import sbtversionpolicy.SbtVersionPolicyPlugin.autoImport.{Compatibility, versionPolicyIntention}


object Versioning extends AutoPlugin {
  val BumpMinor = Compatibility.BinaryAndSourceCompatible
  val BumpMajor = Compatibility.BinaryCompatible
  val BumpEpoch = Compatibility.None

  def currentRef(dir: File): String =
    new DefaultReadableGit(dir, None)
      .withGit(g => g.currentTags.headOption.getOrElse(g.branch))

  object VersionInfo {
    sealed trait VersionType
    object VersionType {
      case object Devel extends VersionType
      sealed trait Tag extends VersionType {
        val x, y, z: Version.Numeric
        def versionString = s"${x.repr}.${y.repr}.${z.repr}"
      }
      object Tag {
        case class Prerelease(x: Version.Numeric, y: Version.Numeric, z: Version.Numeric) extends Tag
        case class Stable(x: Version.Numeric, y: Version.Numeric, z: Version.Numeric) extends Tag {
          def bumpMinor = copy(z = z.next)
          def bumpMajor = copy(y = y.next, z = Version.Number(0))
          def bumpEpoch = copy(x = x.next, y = Version.Number(0), z = Version.Number(0))
          def bump(compat: Compatibility) = compat match {
            case BumpMinor => bumpMinor
            case BumpMajor => bumpMajor
            case BumpEpoch => bumpEpoch
          }
        }
      }
    }
  }
  case class VersionInfo(compat: Compatibility, gitDescribeOutput: GitDescribeOutput) {
    private val cleanAfterTag = gitDescribeOutput.isCleanAfterTag
    private val lastTag = gitDescribeOutput.ref.dropPrefix

    private def versionType(version: Version): VersionInfo.VersionType.Tag = version.items match {
      case Seq(x: Version.Numeric, y: Version.Numeric, z: Version.Numeric, t: Version.Tag, _: Version.Numeric)
        if t.isPreRelease                                                  =>
        VersionInfo.VersionType.Tag.Prerelease(x, y, z)
      case Seq(x: Version.Numeric, y: Version.Numeric, z: Version.Numeric) =>
        VersionInfo.VersionType.Tag.Stable(x, y, z)
      case other                                                           =>
        sys.error(s"Unhandled version format: $other")
    }

    private val lastTagVersion: VersionType.Tag = versionType(Version(lastTag))

    def versionType =
      if (cleanAfterTag)
        lastTagVersion
      else
        VersionInfo.VersionType.Devel

    def nextVersion = lastTagVersion match {
      case v @ VersionInfo.VersionType.Tag.Prerelease(x, y, z) => v.versionString
      case v @ VersionInfo.VersionType.Tag.Stable(x, y, z)     => v.bump(compat).versionString
    }

    def versionString =
      if (cleanAfterTag) lastTag
      else
        nextVersion +
          s"-pre.${gitDescribeOutput.commitSuffix.distance}.${gitDescribeOutput.commitSuffix.sha}" +
          (if (gitDescribeOutput.isDirty()) ".dirty" else "")

    def shortVersionString =
      if (cleanAfterTag) lastTag
      else nextVersion + "-SNAPSHOT"
  }

  case class MaybeVersionInfo(compat: Compatibility, date: Date, maybeGitDescribeOutput: Option[GitDescribeOutput]) {
    def maybeVersionInfo = maybeGitDescribeOutput.map(VersionInfo(compat, _))
    def versionString = maybeVersionInfo.fold(s"HEAD-${DynVer.timestamp(date)}")(_.versionString)
  }

  val maybeVersionInfo = settingKey[MaybeVersionInfo]("Versioning.VersionInfo instance")

  def versionFor(compat: Compatibility, date: Date = new Date): Option[String] =
    DynVer.getGitDescribeOutput(date)
      .map(VersionInfo(compat, _).versionString)

  def shortVersionFor(compat: Compatibility, date: Date = new Date): Option[String] =
    DynVer.getGitDescribeOutput(date)
      .map(VersionInfo(compat, _).shortVersionString)

  override def trigger = allRequirements

  override def projectSettings =
    List(
      maybeVersionInfo :=
        MaybeVersionInfo(versionPolicyIntention.value, dynverCurrentDate.value, dynverGitDescribeOutput.value),
      version := maybeVersionInfo.value.versionString,
      dynver := {
        val d = new Date
        MaybeVersionInfo(versionPolicyIntention.value, d, DynVer.getGitDescribeOutput(d)).versionString
      }
    )
}
