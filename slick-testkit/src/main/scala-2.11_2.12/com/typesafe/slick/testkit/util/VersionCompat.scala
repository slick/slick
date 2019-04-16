package com.typesafe.slick.testkit.util

private[testkit] object VersionCompat {

  def standardInterpolator(process: String => String, args: scala.collection.Seq[Any], sc: StringContext): String =
    sc.standardInterpolator(process, args)
}
