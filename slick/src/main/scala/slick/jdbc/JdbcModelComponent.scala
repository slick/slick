package slick.jdbc

import scala.concurrent.ExecutionContext
import slick.dbio.DBIO
import slick.jdbc.meta.MTable
import slick.model.Model

trait JdbcModelComponent { self: JdbcProfile =>
  /** Jdbc meta data for all tables included in the Slick model by default */
  def defaultTables(implicit ec: ExecutionContext): DBIO[Seq[MTable]] = MTable.getTables

  /** Gets the Slick data model describing this data source
    * @param tables used to build the model, uses defaultTables if None given
    * @param ignoreInvalidDefaults logs unrecognized default values instead of throwing an exception */
  def createModel(tables: Option[DBIO[Seq[MTable]]] = None, ignoreInvalidDefaults: Boolean = true)(implicit ec: ExecutionContext): DBIO[Model] = {
    val tablesA = tables.getOrElse(defaultTables)
    tablesA.flatMap(t => createModelBuilder(t, ignoreInvalidDefaults).buildModel)
  }

  def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext): JdbcModelBuilder =
    new JdbcModelBuilder(tables, ignoreInvalidDefaults)
}
