import com.typesafe.sbt.git.DefaultReadableGit
import coursier.version.Version
import sbt.AutoPlugin
import sbt.Keys.version
import sbtdynver.DynVerPlugin.autoImport.{dynver, dynverCurrentDate, dynverGitDescribeOutput}
import sbtdynver.{DynVer, GitDescribeOutput}
import sbtversionpolicy.SbtVersionPolicyPlugin.autoImport.{Compatibility, versionPolicyIntention}

import java.io.File
import java.util.Date


object Versioning extends AutoPlugin {
  val BumpMinor = Compatibility.BinaryAndSourceCompatible
  val BumpMajor = Compatibility.BinaryCompatible
  val BumpEpoch = Compatibility.None

  def currentRef(dir: File) =
    new DefaultReadableGit(dir)
      .withGit(g => g.currentTags.headOption.getOrElse(g.branch))

  def versionFor(compat: Compatibility, out: GitDescribeOutput): String = {
    val cleanAfterTag = out.isCleanAfterTag
    val lastTag = out.ref.dropPrefix
    if (cleanAfterTag) lastTag
    else {
      val lastVersion = Version(lastTag)
      val nextVersionParts =
        lastVersion.items match {
          case Seq(x: Version.Number, y: Version.Numeric, z: Version.Numeric, t: Version.Tag, _: Version.Numeric)
            if t.isPreRelease                                                 =>
            Seq(x.repr, y.repr, z.repr)
          case Seq(x: Version.Number, y: Version.Numeric, z: Version.Numeric) =>
            compat match {
              case BumpMinor => Seq(x.repr, y.repr, z.next.repr)
              case BumpMajor => Seq(x.repr, y.next.repr, "0")
              case BumpEpoch => Seq(x.next.repr, "0", "0")
            }
          case other                                                          =>
            sys.error(s"Unhandled version format: $other")
        }

      nextVersionParts.mkString(".") +
        s"-pre.${out.commitSuffix.distance}.${out.commitSuffix.sha}" +
        (if (out.isDirty()) ".dirty" else "")
    }
  }

  def versionFor(compat: Compatibility, date: Date = new Date): Option[String] =
    DynVer.getGitDescribeOutput(date).map(versionFor(compat, _))

  override def trigger = allRequirements

  override def projectSettings = {
    def fallbackVersion(d: Date): String = s"HEAD-${DynVer.timestamp(d)}"

    List(
      version :=
        dynverGitDescribeOutput.value.mkVersion(
          versionFor(versionPolicyIntention.value, _),
          fallbackVersion(dynverCurrentDate.value)
        ),
      dynver := {
        val d = new Date
        DynVer.getGitDescribeOutput(d)
          .mkVersion(versionFor(versionPolicyIntention.value, _), fallbackVersion(d))
      }
    )
  }
}
