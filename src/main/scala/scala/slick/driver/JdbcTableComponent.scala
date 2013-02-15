package scala.slick.driver

import scala.slick.lifted._
import scala.slick.ast._
import scala.slick.ast.Select
import scala.slick.ast.Ref
import FunctionSymbolExtensionMethods._

trait JdbcTableComponent { driver: JdbcDriver =>

  class JdbcColumnOptions {
    val NotNull = ColumnOption.NotNull
    val Nullable = ColumnOption.Nullable
    val PrimaryKey = ColumnOption.PrimaryKey
    def Default[T](defaultValue: T) = ColumnOption.Default[T](defaultValue)
    def DBType(dbType: String) = ColumnOption.DBType(dbType)
    val AutoInc = ColumnOption.AutoInc
  }

  val columnOptions: JdbcColumnOptions = new JdbcColumnOptions

  abstract class Table[T](_schemaName: Option[String], _tableName: String) extends AbstractTable[T](_schemaName, _tableName) { table =>
    def this(_tableName: String) = this(None, _tableName)

    val O: driver.columnOptions.type = columnOptions

    def column[C](n: String, options: ColumnOption[C]*)(implicit tm: TypedType[C]): Column[C] = new Column[C] {
      override def nodeDelegate =
        Select(Node(table) match {
          case r: Ref => r
          case _ => Ref(Node(table).nodeIntrinsicSymbol)
        }, FieldSymbol(n)(options, tm)).nodeTyped(tm)
      override def toString = (Node(table) match {
        case r: Ref => "(" + _tableName + " " + r.sym.name + ")"
        case _ => _tableName
      }) + "." + n
    }

    def createFinderBy[P](f: (this.type => Column[P]))(implicit tm: TypedType[P]): QueryTemplate[P,T] = {
      import driver.Implicit._
      val thisQ = tableToQuery(this).asInstanceOf[Query[this.type, this.type]]
      for {
        param <- Parameters[P]
        table <- thisQ if Library.==.column[Boolean](Node(f(table)), Node(param))
      } yield table
    }

    def ddl: DDL = driver.buildTableDDL(this)

    def tpe = CollectionType(CollectionTypeConstructor.default,  *.tpe)
  }
}
