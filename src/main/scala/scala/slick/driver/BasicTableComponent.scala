package scala.slick.driver

import scala.slick.ql._
import scala.slick.ast.{Library, Node}

trait BasicTableComponent { driver: BasicDriver =>

  class BasicColumnOptions {
    val NotNull = ColumnOption.NotNull
    val Nullable = ColumnOption.Nullable
    val PrimaryKey = ColumnOption.PrimaryKey
    def Default[T](defaultValue: T) = ColumnOption.Default[T](defaultValue)
    def DBType(dbType: String) = ColumnOption.DBType(dbType)
    val AutoInc = ColumnOption.AutoInc
  }

  val columnOptions: BasicColumnOptions = new BasicColumnOptions

  abstract class Table[T](_schemaName: Option[String], _tableName: String) extends AbstractTable[T](_schemaName, _tableName) {
    def this(_tableName: String) = this(None, _tableName)

    val O: driver.columnOptions.type = columnOptions

    def column[C : TypeMapper](n: String, options: ColumnOption[C]*) = NamedColumn[C](Node(this), n, options)

    def createFinderBy[P](f: (this.type => NamedColumn[P]))(implicit tm: TypeMapper[P]): BasicQueryTemplate[P,T] = {
      import driver.Implicit._
      val thisQ = tableToQuery(this).asInstanceOf[Query[this.type, this.type]]
      for {
        param <- Parameters[P]
        table <- thisQ if Library.==.column[Boolean](Node(f(table)), Node(param))
      } yield table
    }

    def ddl: DDL = driver.buildTableDDL(this)
  }
}
