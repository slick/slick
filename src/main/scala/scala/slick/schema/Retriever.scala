package scala.slick.schema

import scala.Option.option2Iterable
import scala.slick.driver.JdbcDriver
import scala.slick.jdbc.JdbcBackend
import scala.collection.mutable.ArrayBuffer
import scala.slick.schema.naming.Naming
import scala.slick.jdbc.meta.MQName
import scala.slick.jdbc.meta.MTable
import scala.slick.backend.DatabaseComponent
import scala.reflect.api.Universe
import scala.slick.typeproviders.TypeMapper

/**
 * This module retrieves the meta model for a given database
 */
object Retriever {

  def tablesWithoutColumn(driver: JdbcDriver)(naming: Naming)(implicit session: JdbcBackend#Session): List[Table] = {
    val tables = (driver.getTables.list map (t => {
      val qualifiedName = QualifiedName(t.name)
      Table(qualifiedName, Nil, Nil, naming.tableSQLToModule(qualifiedName), naming.tableSQLToEntity(qualifiedName))
    }))
    tables
  }

  def tablesWithColumns(driver: JdbcDriver, universe: Universe)(naming: Naming, typeMapper: TypeMapper)(tablesWOColumns: List[Table])(implicit session: JdbcBackend#Session): List[Table] = {
    tablesWOColumns map (t => {
      val mqName = mqNameForTable(t)
      val mTable = MTable.getTables(mqName.catalog, mqName.schema, Some(mqName.name), None).first
      val caseFieldName = naming.columnSQLToEntityField _
      val moduleFieldName = naming.columnSQLToModuleField _
      val mColumns = mTable.getColumns.list
      def columnName(column: String): QualifiedName = QualifiedName.columnName(t.name, column)
      val columns = mColumns map (c => {
        val qualifiedName = columnName(c.column)
        val tpe = typeMapper.columnType(qualifiedName)(universe) match {
          case Some(t) => t
          case _ => driver.scalaTypeForColumn(universe)(c)
        }
        Column(qualifiedName, tpe, moduleFieldName(qualifiedName), caseFieldName(qualifiedName))
      })
      val autoInc = mColumns.zip(columns).collectFirst { case (m, c) if m.isAutoInc.getOrElse(false) => AutoIncrement(c) }
      val constraints = autoInc.toList
      t.copy(columns = columns, constraints = constraints)
    })
  }

  def tablesWithConstraints(naming: Naming)(tablesWColumns: List[Table])(implicit session: JdbcBackend#Session): List[Table] = {
    def getTable(tableName: String): Option[Table] =
      tablesWColumns.find {
        case t =>
          t.name.lastPart equals tableName
      }
    def getSchema(tableName: String): Table =
      getTable(tableName).get
    def getColumnOfTable(tableName: String)(columnName: String): Option[Column] =
      getTable(tableName).map(s => getColumnOfSchema(s)(columnName)).head
    def getColumnOfSchema(schema: Table)(columnName: String): Option[Column] =
      schema.columns.find(c => c.name.lastPart equals columnName)
    val tablesWithConstraint = tablesWColumns map {
      case schema =>
        {
          val mqName = mqNameForTable(schema)
          val mTable = MTable.getTables(mqName.catalog, mqName.schema, Some(mqName.name), None).first
          def getColumn: String => Option[Column] = getColumnOfSchema(schema) _
          val constraints: ArrayBuffer[Constraint] = new ArrayBuffer
          // initialize constraints
          constraints ++= schema.constraints
          // handles primary key
          val primaryKeys = mTable.getPrimaryKeys.list flatMap (p => getColumn(p.column))
          val pkConstraint =
            primaryKeys match {
              case Nil => None
              case list => Some(PrimaryKey(list))
            }
          constraints ++= pkConstraint
          // handles foreign keys
          val fks = mTable.getImportedKeys.list
          val fksGrouped = fks.groupBy(x =>
            (QualifiedName(x.pkTable), QualifiedName(x.fkTable))).mapValues(v => {
            val fields =
              v.map(x => {
                val pkColumn = getColumnOfTable(x.pkTable.name)(x.pkColumn).get
                val fkColumn = getColumnOfTable(x.fkTable.name)(x.fkColumn).get
                (pkColumn, fkColumn)
              })
            val head = v.head
            val (updateRule, deleteRule) = (head.updateRule, head.deleteRule)
            (fields, updateRule, deleteRule)
          })
          val fkConstraints = fksGrouped.map(fk => ForeignKey(fk._1._1, fk._1._2, fk._2._1, fk._2._2, fk._2._3)).toList
          constraints ++= fkConstraints
          // handles indices
          val indices = mTable.getIndexInfo(true, false).list
          val idxGrouped = indices.groupBy(_.indexName).mapValues(_.map(idx => getColumn(idx.columnName.get).get))
          val idxConstraints = idxGrouped.map(idx => Index(idx._2)).toList
          constraints ++= idxConstraints
          schema.copy(constraints = constraints.toList)
        }
    }
    tablesWithConstraint
  }

  def mqNameForTable(table: Table): MQName = {
    import table.name
    val catalog = name.getPart(CatalogName).map(_.name)
    val schema = name.getPart(SchemaName).map(_.name)
    MQName(catalog, schema, name.lastPart)
  }

  def tables(driver: JdbcDriver, db: JdbcBackend#DatabaseDef, universe: Universe)(naming: Naming, typeMapper: TypeMapper): List[Table] = {
    import driver.simple.Database.threadLocalSession
    db withSession {
      val tablesWOColumns = tablesWithoutColumn(driver)(naming)
      val tablesWColumns = tablesWithColumns(driver, universe)(naming, typeMapper)(tablesWOColumns)
      val tablesWConstraints = tablesWithConstraints(naming)(tablesWColumns)
      tablesWConstraints
    }
  }
}
