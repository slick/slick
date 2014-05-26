package scala.slick.mongodb

import scala.slick.profile.RelationalDriver
import scala.slick.ast.Node
import scala.slick.compiler.QueryCompiler

/**
 * User: Dmytro Vynokurov
 * Date: 22.05.14
 * Time: 22:24
 */
trait MongoDriver extends RelationalDriver with MongoProfile{
  override val profile:MongoProfile = this
}
object MongoDriver extends MongoDriver with MongoProfile {
}

