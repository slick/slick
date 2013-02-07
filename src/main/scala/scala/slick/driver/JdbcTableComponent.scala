package scala.slick.driver

import scala.slick.lifted._
import scala.slick.ast._
import FunctionSymbolExtensionMethods._
import scala.slick.profile.RelationalTableComponent

trait JdbcTableComponent extends RelationalTableComponent { driver: JdbcDriver =>

  trait ColumnOptions extends super.ColumnOptions {
    val NotNull = ColumnOption.NotNull
    val Nullable = ColumnOption.Nullable
    def DBType(dbType: String) = ColumnOption.DBType(dbType)
  }

  override val columnOptions: ColumnOptions = new AnyRef with ColumnOptions

  abstract class Table[T](_schemaName: Option[String], _tableName: String) extends super.Table[T](_schemaName, _tableName) {
    def this(_tableName: String) = this(None, _tableName)

    def createFinderBy[P](f: (this.type => Column[P]))(implicit tm: TypedType[P]): QueryTemplate[P,T] = {
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
