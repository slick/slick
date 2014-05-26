package scala.slick.mongodb

import scala.slick.profile.{RelationalDriver, RelationalProfile, BasicProfile, Capability}
import scala.slick.ast.Node
import scala.slick.compiler.QueryCompiler
import scala.slick.driver.JdbcTypesComponent.ImplicitColumnTypes
import scala.slick.profile.SqlProfile.DDL
import scala.slick.driver.JdbcInvokerComponent.DDLInvoker
import scala.slick.lifted.{ColumnBase, MappedProjection, RunnableCompiled, Query}
import scala.slick.driver.JdbcInvokerComponent.DeleteInvoker
import scala.slick.driver.JdbcInvokerComponent.QueryInvoker
import scala.slick.driver.JdbcInvokerComponent.UpdateInvoker
import scala.slick.jdbc.JdbcMappingCompilerComponent.JdbcFastPathExtensionMethods

/**
 * User: Dmytro Vynokurov
 * Date: 22.05.14
 * Time: 19:40
 */
trait MongoProfile extends RelationalProfile { driver:MongoDriver =>
  type Backend = MongoBackend
}
object MongoProfile extends MongoProfile {

}