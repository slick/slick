package scala.slick.schema

import scala.Option.option2Iterable
import scala.slick.driver.JdbcDriver
import scala.slick.jdbc.JdbcBackend

object Retriever {
  def tables(driver: JdbcDriver, db: JdbcBackend#DatabaseDef): List[Table] = {
    import driver.simple.Database.threadLocalSession
    db withSession {
      import Column._
      val tables = (driver.getTables.list map (t => {
        val columns = t.getColumns.list map (c => Column(c.column, driver.scalaTypeForColumn(c)))
        (t, Table(t.name.name, columns))
      }))
      def getTable(tableName: String): Option[Table] =
        tables.find {
          case (t, s) =>
            s.table equals tableName
        }.map(_._2)
      def getSchema(tableName: String): Table =
        getTable(tableName).get
      def getColumnOfTable(tableName: String)(columnName: String): Option[Column] =
        getTable(tableName).map(s => getColumnOfSchema(s)(columnName)).head
      def getColumnOfSchema(schema: Table)(columnName: String): Option[Column] =
        schema.columns.find(c => c.name equals columnName)
      tables foreach {
        case (t, schema) =>
          {
            def getColumn: String => Option[Column] = getColumnOfSchema(schema) _
            // handles primary key
            val primaryKeys = t.getPrimaryKeys.list flatMap (p => getColumn(p.column))
            val pkConstraint = PrimaryKey(primaryKeys)
            schema += pkConstraint
            // handles foreign keys
            val fks = t.getImportedKeys.list
            val fksGrouped = fks.groupBy(x =>
              (getSchema(x.pkTable.name), getSchema(x.fkTable.name))).mapValues(v => {
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
            schema ++= fkConstraints
            // handles indices
            val indices = t.getIndexInfo(true, false).list
            val idxGrouped = indices.groupBy(_.indexName).mapValues(_.map(idx => getColumn(idx.columnName.get).get))
            val idxConstraints = idxGrouped.map(idx => Index(idx._2)).toList
            schema ++= idxConstraints
          }
      }
      tables.map(_._2)
    }
  }
}
