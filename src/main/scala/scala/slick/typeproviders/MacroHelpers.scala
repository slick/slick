package scala.slick.typeproviders

import scala.reflect.macros.Context
import scala.slick.schema.Table
import scala.slick.schema.Column
import scala.slick.schema.ForeignKey
import scala.slick.schema.Index
import scala.slick.schema.naming.Naming
import scala.slick.schema.PrimaryKey

abstract class MacroHelpers(val naming: Naming) {
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

  // helper methods for creating the module for each table
  def getColumnOfTable(tableName: String)(c: Column) = Select(This(TypeName(tableName)), TermName(c.moduleFieldName))

  def columnToMethodDef(tableName: String)(column: Column)(isAutoInc: Boolean): DefDef = {
    val nameParam = Literal(Constant(column.name.lastPart))
    val autoIncParam =
      if (isAutoInc)
        Some(Select(Select(This(TypeName(tableName)), TermName("O")), TermName("AutoInc")))
      else
        None
    val params = nameParam :: autoIncParam.toList
    DefDef(NoMods, TermName(column.moduleFieldName), List(), List(), TypeTree(), Apply(TypeApply(Select(This(TypeName(tableName)), TermName("column")), List(columnToType(column))), params))
  }

  def starDef(tableName: String)(columns: List[Column], caseClassName: TermName): DefDef = {
    val allFields = columns.map(getColumnOfTable(tableName)).reduceLeft[Tree]((prev, current) => {
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
          {
            val xVar = context.freshName("x")
            Function(
              List(ValDef(Modifiers(PARAM | SYNTHETIC), TermName(xVar), TypeTree(), EmptyTree)),
              Apply(Select(Ident(caseClassName), TermName("unapply")), List(Ident(TermName(xVar)))))
          })))
  }

  def primaryKeyDef(tableName: String)(pk: PrimaryKey): DefDef = {
    val methodName = naming.primaryKeyName(pk)
    val pks = createTupleOrSingleton(pk.fields map getColumnOfTable(tableName))
    DefDef(NoMods, TermName(methodName), List(), List(), TypeTree(), Apply(Select(This(TypeName(tableName)), TermName("primaryKey")), List(Literal(Constant("CONSTRAINT_PK_" + tableName.toUpperCase)), pks)))
  }

  def foreignKeyDef(tableName: String)(fk: ForeignKey): DefDef = {
    def getColumnOfName(n: String)(c: Column) = Select(Ident(TermName(n)), TermName(c.moduleFieldName))
    val methodName = naming.foreignKeyName(fk)
    val fkName = Literal(Constant(methodName))
    val fkFkColumns = fk.fields.map(_._2)
    val fkSourceColumns = createTupleOrSingleton(fkFkColumns map getColumnOfTable(tableName))
    val fkTargetTable = Ident(TermName(naming.tableSQLToModule(fk.pkTableName)))
    val fkPkColumns = fk.fields.map(_._1)
    val SYNTHETIC = scala.reflect.internal.Flags.SYNTHETIC.asInstanceOf[Long].asInstanceOf[FlagSet]
    val fkTargetColumns = Function(List(ValDef(Modifiers(PARAM | SYNTHETIC), TermName("x"), TypeTree(), EmptyTree)), createTupleOrSingleton(fkPkColumns map (getColumnOfName("x"))))
    val fkUpdateRule = Select(foreignKeyActionObject, TermName(fk.updateRule.toString))
    val fkDeleteRule = Select(foreignKeyActionObject, TermName(fk.deleteRule.toString))
    DefDef(NoMods, TermName(methodName), List(), List(), TypeTree(), Apply(Apply(Select(This(TypeName(tableName)), TermName("foreignKey")), List(fkName, fkSourceColumns, fkTargetTable)), List(fkTargetColumns, fkUpdateRule, fkDeleteRule)))
  }

  def indexDef(tableName: String)(idx: Index): DefDef = {
    val methodName = naming.indexName(idx)
    val idxName = Literal(Constant(methodName))
    val idxOn = createTupleOrSingleton(idx.fields map getColumnOfTable(tableName))
    val idxUnique = Literal(Constant(true))
    DefDef(NoMods, TermName(methodName), List(), List(), TypeTree(), Apply(Select(This(TypeName(tableName)), TermName("index")), List(idxName, idxOn, idxUnique)))
  }

  // creates case class for each table
  def tableToCaseClass(table: Table): ClassDef = {
    val columns = table.columns
    val schema = columns map (column => (column.caseFieldName, columnToType(column)))
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
    val tableName = table.moduleName
    val tableType = createClass("Table", Nil)
    val tableSuper = AppliedTypeTree(tableType, List(Ident(caseClass.name)))
    val superCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List(Literal(Constant(table.name.lastPart))))
    val constructor = DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(superCall), Literal(Constant(()))))
    val autoIncField = table.autoInc.map(_.field)
    val fields = columns map { c =>
      columnToMethodDef(tableName)(c)(autoIncField.exists(f => f equals c))
    }
    val star = starDef(tableName)(columns, caseClass.name.toTermName)
    val pk = table.primaryKey map primaryKeyDef(tableName)
    val fks = table.foreignKeys map foreignKeyDef(tableName)
    val idx = table.indices map indexDef(tableName)
    val methods = constructor :: (fields ++ (star :: (pk.toList ::: fks ::: idx)))
    ModuleDef(NoMods, TermName(tableName), Template(List(tableSuper), emptyValDef, methods))
  }

}
