package scala.slick.direct

import scala.annotation.StaticAnnotation

final case class table(name:String) extends StaticAnnotation
final case class column(name:String) extends StaticAnnotation
