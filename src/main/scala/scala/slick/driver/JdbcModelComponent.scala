package scala.slick.driver

import scala.concurrent.{ExecutionContext, Future}
import scala.slick.dbio.DBIO
import scala.slick.jdbc.JdbcModelBuilder
import scala.slick.jdbc.meta.{MTable, MTableExtended}
import scala.slick.model.Model

trait JdbcModelComponent { driver: JdbcDriver =>
  /** Jdbc meta data for all tables included in the Slick model by default */
  def defaultTables(implicit ec: ExecutionContext): DBIO[Seq[MTableExtended]] = MTable.getExtendedTables

  /** Gets the Slick data model describing this data source
    * @param tables used to build the model, uses defaultTables if None given
    * @param ignoreInvalidDefaults logs unrecognized default values instead of throwing an exception */
  def createModel(tables: Option[DBIO[Seq[MTableExtended]]] = None, ignoreInvalidDefaults: Boolean = true)(implicit ec: ExecutionContext): DBIO[Model] = {
    val tablesA = tables.getOrElse(defaultTables)
    tablesA.map(t => createModelBuilder(t, ignoreInvalidDefaults).createModel)
  }

  def createModelBuilder(tables: Seq[MTableExtended], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext): JdbcModelBuilder =
    new JdbcModelBuilder(tables, ignoreInvalidDefaults)
}
