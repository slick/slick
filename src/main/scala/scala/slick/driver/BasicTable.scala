package scala.slick.driver

import scala.slick.ql._
import scala.slick.ast.Node

abstract class AbstractBasicTable[T](_schemaName: Option[String], _tableName: String) extends AbstractTable[T](_schemaName, _tableName) {

  type ProfileType <: BasicProfile

  val O: BasicColumnOptions = BasicColumnOptions

  def column[C : TypeMapper](n: String, options: ColumnOption[C, ProfileType]*) = NamedColumn[C](Node(this), n, options)

  def createFinderBy[P](f: (this.type => NamedColumn[P]))(implicit driver: BasicProfile, tm: TypeMapper[P]): BasicQueryTemplate[P,T] = {
    import driver.Implicit.{slickDriver => _, _}
    for {
      param <- Parameters[P]
      table <- this if ColumnOps.Is(Node(f(this)), Node(param))
    } yield table
  }

  def ddl(implicit profile: ProfileType): DDL = profile.buildTableDDL(this)
}

abstract class BasicTable[T](_schemaName: Option[String], _tableName: String) extends AbstractBasicTable[T](_schemaName, _tableName) {
  def this(_tableName: String) = this(None, _tableName)
  type ProfileType = BasicProfile
}
