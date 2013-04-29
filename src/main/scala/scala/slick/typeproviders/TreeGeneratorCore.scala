package scala.slick.typeproviders

import scala.slick.schema.Table
import scala.slick.schema.Column
import scala.slick.schema.ForeignKey
import scala.slick.schema.Index
import scala.slick.schema.naming.Naming
import scala.slick.schema.PrimaryKey
import scala.slick.SlickException
import scala.reflect.api.Universe
import scala.language.reflectiveCalls
import scala.slick.schema.Retriever

trait TreeGeneratorCore { self: MacroHelpers =>
  import universe._
  import Flag._

  // helper methods for creating case class for each table
  /**
   * Converts Column to its type tree
   */
  def columnToType(column: Column): Tree = typeToTree(column.tpe.asInstanceOf[Type])

  // helper methods for creating the module for each table
  /**
   * Gets tree representation of a column when it is accessed
   */
  def getColumnOfTable(tableName: String)(c: Column) = Select(This(TypeName(tableName)), TermName(c.moduleFieldName))

  /**
   * Gets method definition of a column in Lifted Embedding
   */
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

  /**
   * Gets method definition for `*` of a Table in Lifted Embedding
   */
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
            val xVar = contextUtils.freshName("x")
            Function(
              List(ValDef(Modifiers(PARAM | SYNTHETIC), TermName(xVar), TypeTree(), EmptyTree)),
              //              List(ValDef(Modifiers(PARAM | SYNTHETIC), TermName(xVar), Ident(caseClassName.toTypeName), EmptyTree)),
              Apply(Select(Ident(caseClassName), TermName("unapply")), List(Ident(TermName(xVar)))))
          })))
  }

  /**
   * Gets method definition for a primary key in Lifted Embedding
   */
  def primaryKeyDef(tableName: String)(pk: PrimaryKey): DefDef = {
    val methodName = naming.primaryKeyName(pk)
    val pks = createTupleOrSingleton(pk.fields map getColumnOfTable(tableName))
    DefDef(NoMods, TermName(methodName), List(), List(), TypeTree(), Apply(Select(This(TypeName(tableName)), TermName("primaryKey")), List(Literal(Constant("CONSTRAINT_PK_" + tableName.toUpperCase)), pks)))
  }

  /**
   * Gets method definition for a foreign key in Lifted Embedding
   */
  def foreignKeyDef(tableName: String)(fk: ForeignKey): DefDef = {
    def getColumnOfName(n: String)(c: Column) = Select(Ident(TermName(n)), TermName(c.moduleFieldName))
    val methodName = naming.foreignKeyName(fk)
    val fkName = Literal(Constant(methodName))
    val fkFkColumns = fk.fields.map(_._2)
    val fkSourceColumns = createTupleOrSingleton(fkFkColumns map getColumnOfTable(tableName))
    val fkTargetTable = Ident(TermName(naming.tableSQLToModule(fk.pkTableName)))
    val fkPkColumns = fk.fields.map(_._1)
    val SYNTHETIC = scala.reflect.internal.Flags.SYNTHETIC.asInstanceOf[Long].asInstanceOf[FlagSet]
    val xVar = contextUtils.freshName("x")
    val fkTargetColumns = Function(List(ValDef(Modifiers(PARAM | SYNTHETIC), TermName(xVar), TypeTree(), EmptyTree)), createTupleOrSingleton(fkPkColumns map (getColumnOfName(xVar))))
    val fkUpdateRule = Select(foreignKeyActionObject, TermName(fk.updateRule.toString))
    val fkDeleteRule = Select(foreignKeyActionObject, TermName(fk.deleteRule.toString))
    DefDef(NoMods, TermName(methodName), List(), List(), TypeTree(), Apply(Apply(Select(This(TypeName(tableName)), TermName("foreignKey")), List(fkName, fkSourceColumns, fkTargetTable)), List(fkTargetColumns, fkUpdateRule, fkDeleteRule)))
  }

  /**
   * Gets method definition for an index in Lifted Embedding
   */
  def indexDef(tableName: String)(idx: Index): DefDef = {
    val methodName = naming.indexName(idx)
    val idxName = Literal(Constant(methodName))
    val idxOn = createTupleOrSingleton(idx.fields map getColumnOfTable(tableName))
    val idxUnique = Literal(Constant(true))
    DefDef(NoMods, TermName(methodName), List(), List(), TypeTree(), Apply(Select(This(TypeName(tableName)), TermName("index")), List(idxName, idxOn, idxUnique)))
  }

  // creates case class for each table
  /**
   * Converts a table meta-model into its case class definition
   */
  def tableToCaseClass(table: Table): ClassDef = {
    val columns = table.columns
    val schema = columns map (column => (column.caseFieldName, columnToType(column)))
    val caseClassName = table.caseClassName

    import CaseClassCreator._

    val fields: List[ValDef] = schema map { case (name, tpe) => fieldCreate(name, tpe) }
    val ctorParams: List[ValDef] = schema map { case (name, tpe) => ctorParamCreate(name, tpe) }
    val ctor: DefDef = ctorCreate(ctorParams)

    caseClassCreate(caseClassName, fields, ctor)
  }

  // creates type for each table
  /**
   * Converts a table meta-model to its type alias
   */
  def tableToType(table: Table)(tpe: Type): TypeDef = {
    val typeName = table.caseClassName
    val typeType = typeToTree(tpe.normalize)
    TypeDef(NoMods, TypeName(typeName), List(), typeType)
  }

  /**
   * Converts a table meta-model to its extractor (Its apply and unapply are used for <> of * in Lifted Embedding)
   */
  def tableToTypeVal[Elem, TupleElem](table: Table)(obj: Type, typeParamName: TypeName): ValDef = {
    val termName = table.caseClassName
    val tpe = obj.asInstanceOf[Type]
    val typeTree = typeToTree(tpe)
    val rhs = Apply(Select(New(typeTree), nme.CONSTRUCTOR), List())
    ValDef(NoMods, TermName(termName), TypeTree(), rhs)
  }

  // creates module for each table
  /**
   * Converts a table meta-model to its Table class definition in Lifted Embedding
   */
  def tableToModule(table: Table): ModuleDef = {
    val typeParamName = TypeName(table.caseClassName)
    val columns = table.columns
    val tableName = table.moduleName
    val tableType = createClass("Table", Nil)
    val tableSuper = AppliedTypeTree(tableType, List(Ident(typeParamName)))
    val superCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List(Literal(Constant(table.name.lastPart))))
    val constructor = DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(superCall), Literal(Constant(()))))
    val autoIncField = table.autoInc.map(_.field)
    val fields = columns map { c =>
      columnToMethodDef(tableName)(c)(autoIncField.exists(f => f equals c))
    }
    val star = starDef(tableName)(columns, typeParamName.toTermName)
    val pk = table.primaryKey map primaryKeyDef(tableName)
    val fks = table.foreignKeys map foreignKeyDef(tableName)
    val idx = table.indices map indexDef(tableName)
    val methods = constructor :: (fields ++ (star :: (pk.toList ::: fks ::: idx)))
    ModuleDef(NoMods, TermName(tableName), Template(List(tableSuper), emptyValDef, methods))
  }

  /**
   * The main method for getting tree representation for all tables.
   */
  def generateTreeForTables: List[Tree] = {
    val db = driver.simple.Database.forURL(urlForConnection, driver = jdbcClass,
      user = userForConnection, password = passForConnection)
    val tables = Retriever.tables(driver, db, universe)(naming, typeMapper._1)
    tables.flatMap(table => {
      // generate the dto case class
      val tableType = typeMapper._1.tableType(table.name)(universe) match {
        case None => tableToCaseClass(table)
        case Some(tpe) => tableToType(table)(tpe)
      }
      // extractor!
      val tableTypeVal = typeMapper._1.tableExtractor(table.name)(universe) match {
        case None => Nil
        case Some(obj) => List(tableToTypeVal(table)(obj, tableType.name))
      }
      // generate the table object
      val tableModule = tableToModule(table)

      tableTypeVal ++ List(tableType, tableModule)
    })
  }

  /**
   * Returns import statements for generated tree
   */
  def getImports: List[Import] = {
    val importSimpleWild = self.createImport(self.createObjectFromString(s"_root_.$slickDriverObject.simple"), Nil)
    val importTypeMapper = typeMapper match {
      case (_, None) => Nil
      case (typeMapperObject, Some((typeMapperName, typeMapperClass))) => {
        val typeMapperImplicits = implicitMembersNameOfType(typeMapperClass.typeSignature)
        def implicitNameConvertor(implicitName: String) = s"typeMapperObject_$implicitName"
        val importTypeMapper = self.createImport(self.createObjectFromString(typeMapperName), typeMapperImplicits, implicitNameConvertor)
        List(importTypeMapper)
      }
    }
    List(importSimpleWild) ++ importTypeMapper
  }

}