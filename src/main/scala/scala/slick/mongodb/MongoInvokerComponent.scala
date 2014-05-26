package scala.slick.mongodb

import scala.slick.profile.BasicInvokerComponent

/**
 * User: Dmytro Vynokurov
 * Date: 26.05.14
 * Time: 21:50
 */
class MongoInvokerComponent extends BasicInvokerComponent{
  /** Create a DDLInvoker -- this method should be implemented by drivers as needed */
  override def createDDLInvoker(ddl: MongoInvokerComponent#SchemaDescription): DDLInvoker =

}
