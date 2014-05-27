package scala.slick.mongodb

import scala.slick.profile.RelationalProfile

/**
 * User: Dmytro Vynokurov
 * Date: 22.05.14
 * Time: 19:40
 */
trait MongoProfile extends RelationalProfile { driver:MongoDriver =>
  type Backend = MongoBackend
}
//object MongoProfile extends MongoProfile {
//
//}