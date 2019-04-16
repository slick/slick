package com.typesafe.slick.testkit.util

private[testkit] object VersionCompat {

  // We may be able to get rid of this before 2.13.0 if https://github.com/scala/scala/pull/7983 is accepted
  def standardInterpolator(process: String => String, args: scala.collection.Seq[Any], sc: StringContext): String =
    StringContext.standardInterpolator(process, args, sc.parts)
}
