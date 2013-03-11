package scala.slick.typeproviders

import scala.reflect.macros.Context
import scala.slick.schema.Table
import scala.slick.schema.Column
import scala.slick.schema.ForeignKey

trait MacroHelpers {
  val context: Context
  import context.universe._
  import Flag._

  // some general methods
  def createObjectOrClass(generator: String => Name)(name: String, packages: scala.List[String]): Tree = {
    val classType = generator(name)
    val packagesType = packages map (TermName(_))
    if (packagesType.isEmpty)
      Ident(classType)
    else {
      val firstPackage = Ident(packagesType.head)
      val others = (packagesType.tail :+ classType)
      others.foldLeft[Tree](firstPackage)((prev, elem) => {
        Select(prev, elem)
      })
    }
  }

  def createClass(className: String, packages: scala.List[String]): Tree =
    createObjectOrClass(x => TypeName(x))(className, packages)

  def createObject(objectName: String, packages: scala.List[String]): Tree =
    createObjectOrClass(x => TermName(x))(objectName, packages)

  def createTuple[T <: Tree](elems: List[T]): Tree =
    Apply(Select(tupleObject(elems.length), TermName("apply")), elems)

  def createTupleOrSingleton[T <: Tree](elems: List[T]): Tree =
    elems match {
      case List(elem) => elem
      case other => createTuple(other)
    }

  // some useful classes and objects
  def tupleObject(arity: Int): Tree = createObject("Tuple" + arity, List("scala"))
  def scalaProductClass: Tree = createClass("Product", List("scala"))
  def scalaSerializableClass: Tree = createClass("Serializable", List("scala"))
  //  def foreignKeyActionObject: Tree = context.parse("scala.slick.lifted.ForeignKeyAction")
  def foreignKeyActionObject: Tree = createObject("ForeignKeyAction", List("scala", "slick", "lifted"))

  // helper for creating a case class
  object CaseClassCreator {
    val CASEACCESSOR = scala.reflect.internal.Flags.CASEACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]
    val PARAMACCESSOR = scala.reflect.internal.Flags.PARAMACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]

    def fieldCreate(name: String, tpt: Tree): ValDef = ValDef(Modifiers(CASEACCESSOR | PARAMACCESSOR), TermName(name), tpt, EmptyTree)
    def ctorParamCreate(name: String, tpt: Tree): ValDef = ValDef(Modifiers(PARAM | PARAMACCESSOR), TermName(name), tpt, EmptyTree)
    def ctorCreate(ctorParams: List[ValDef]): DefDef = DefDef(NoMods, nme.CONSTRUCTOR, List(), List(ctorParams), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(()))))
    def caseClassCreate(name: String, fields: List[ValDef], ctor: DefDef): ClassDef =
      ClassDef(Modifiers(CASE), TypeName(name), Nil, Template(
        List(scalaProductClass, scalaSerializableClass),
        emptyValDef,
        fields :+ ctor))
  }

  // helper methods for creating case class for each table
  def columnToType(column: Column): Ident = Ident(TypeName(column.tpe.toString))

  def columnToMethodDef(tableName: String)(column: Column): DefDef = {
    DefDef(NoMods, TermName(column.scalaName), List(), List(), TypeTree(), Apply(TypeApply(Select(This(TypeName(tableName)), TermName("column")), List(columnToType(column))), List(Literal(Constant(column.name)))))
  }

  // helper methods for creating the module for each table
  def starDef(tableName: String)(columns: List[Column], caseClassName: TermName): DefDef = {
    def getField(column: Column) = Select(This(TypeName(tableName)), TermName(column.scalaName))
    val allFields = columns.map(getField).reduceLeft[Tree]((prev, current) => {
      val tilde = Select(prev, TermName("$tilde"))
      Apply(tilde, List(current))
    })
    val SYNTHETIC = scala.reflect.internal.Flags.SYNTHETIC.asInstanceOf[Long].asInstanceOf[FlagSet]
    DefDef(NoMods,
      TermName("$times"),
      List(),
      List(),
      TypeTree(),
      Apply(
        Select(allFields, TermName("$less$greater")),
        List(
          Ident(caseClassName),
          Function(
            List(ValDef(Modifiers(PARAM | SYNTHETIC), TermName("x$0"), TypeTree(), EmptyTree)),
            Apply(Select(Ident(caseClassName), TermName("unapply")), List(Ident(TermName("x$0"))))))))
  }

  def primaryKeyDef(tableName: String)(columns: List[Column]): DefDef = {
    def getColumn(c: Column) = Select(This(TypeName(tableName)), TermName(c.scalaName))
    val pks = createTupleOrSingleton(columns map getColumn)
    DefDef(NoMods, TermName("primaryKey" + tableName), List(), List(), TypeTree(), Apply(Select(This(TypeName(tableName)), TermName("primaryKey")), List(Literal(Constant("CONSTRAINT_PK_" + tableName.toUpperCase)), pks)))
  }

  def foreignKeyDef(tableName: String)(fk: ForeignKey): DefDef = {
    def getColumn(c: Column) = Select(This(TypeName(tableName)), TermName(c.scalaName))
    def getColumnOfName(n: String)(c: Column) = Select(Ident(TermName(n)), TermName(c.scalaName))
    val fkName = Literal(Constant(fk.pkTable.table + "_fk"))
    val fkFkColumns = fk.fields.map(_._2)
    val fkSourceColumns = createTupleOrSingleton(fkFkColumns map getColumn)
    val fkTargetTable = Ident(TermName(fk.pkTable.scalaName))
    val fkPkColumns = fk.fields.map(_._1)
    val SYNTHETIC = scala.reflect.internal.Flags.SYNTHETIC.asInstanceOf[Long].asInstanceOf[FlagSet]
    val fkTargetColumns = Function(List(ValDef(Modifiers(PARAM | SYNTHETIC), TermName("x"), TypeTree(), EmptyTree)), createTupleOrSingleton(fkPkColumns map (getColumnOfName("x"))))
    val fkUpdateRule = Select(foreignKeyActionObject, TermName(fk.updateRule.toString))
    val fkDeleteRule = Select(foreignKeyActionObject, TermName(fk.deleteRule.toString))
    DefDef(NoMods, TermName("fk" + fk.pkTable.scalaName), List(), List(), TypeTree(), Apply(Apply(Select(This(TypeName(tableName)), TermName("foreignKey")), List(fkName, fkSourceColumns, fkTargetTable)), List(Block(List(), fkTargetColumns), fkUpdateRule, fkDeleteRule)))
  }

  // creates case class for each table
  def tableToCaseClass(table: Table): ClassDef = {
    val columns = table.columns
    val schema = columns map (column => (column.scalaName, columnToType(column)))
    val caseClassName = table.caseClassName

    import CaseClassCreator._

    val fields: List[ValDef] = schema map (fieldCreate _).tupled
    val ctorParams: List[ValDef] = schema map (ctorParamCreate _).tupled
    val ctor: DefDef = ctorCreate(ctorParams)

    caseClassCreate(caseClassName, fields, ctor)
  }

  // creates module for each table
  def tableToModule(table: Table, caseClass: ClassDef): ModuleDef = {
    val columns = table.columns
    val tableName = table.scalaName
    val tableType = createClass("Table", Nil)
    val tableSuper = AppliedTypeTree(tableType, List(Ident(caseClass.name)))
    val superCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List(Literal(Constant(table.table))))
    val constructor = DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(superCall), Literal(Constant(()))))
    val fields = columns map columnToMethodDef(tableName)
    val star = starDef(tableName)(columns, caseClass.name.toTermName)
    val pk = if (table.primaryKeys.isEmpty) None else Some(primaryKeyDef(tableName)(table.primaryKeys))
    val fks = table.foreignKeys map foreignKeyDef(tableName)
    val methods = constructor :: (fields ++ (star :: (pk.toList ::: fks)))
    ModuleDef(NoMods, TermName(tableName), Template(List(tableSuper), emptyValDef, methods))
  }

}