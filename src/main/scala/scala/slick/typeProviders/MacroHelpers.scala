package scala.slick.typeProviders

import scala.reflect.macros.Context

trait MacroHelpers{
	val context: Context
	import context.universe._
	import Flag._

	def columnToType(column: Column) = Ident(TypeName(column.scalaType))

	def createClass(className: String, packages: scala.List[String]) = {
		val classType = TypeName(className)
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
	
	def tableToCaseClass(table: Schema) = {
      // load table schema
      val columns = table.columns
      val schema = columns map (column => (column.scalaName, columnToType(column)))
	  val caseClassName = table.caseClassName
	  val CASEACCESSOR = scala.reflect.internal.Flags.CASEACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]
      val PARAMACCESSOR = scala.reflect.internal.Flags.PARAMACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]
      val fields: List[Tree] = schema map { case (name, tpt) => ValDef(Modifiers(CASEACCESSOR | PARAMACCESSOR), name, tpt, EmptyTree) }
      val ctorParams: List[ValDef] = schema map { case (name, tpt) => ValDef(Modifiers(PARAM | PARAMACCESSOR), name, tpt, EmptyTree) }
      val ctor: Tree = DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(ctorParams), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(()))))
      val caseClass = ClassDef(Modifiers(CASE), TypeName(caseClassName), Nil, Template(
      List(Select(Ident(TermName("scala")), TypeName("Product")), Select(Ident(TermName("scala")), TypeName("Serializable"))),
           emptyValDef,
           fields :+ ctor))
      caseClass
	}
	
	def columnToMethodDef(tableName: String)(column: Column) = {
	  DefDef(Modifiers(), TermName(column.scalaName), List(), List(), TypeTree(), Apply(TypeApply(Select(This(TypeName(tableName)), TermName("column")), List(columnToType(column))), List(Literal(Constant(column.name)))))
	}
	
	def createTuple[T <: Tree](elems: List[T]) = 
//	  Apply(Select(Ident(TermName("scala.Tuple"+elems.length)), TermName("apply")), elems)
	  Apply(Select(context.parse("scala.Tuple"+elems.length), TermName("apply")), elems)
	
    def createTupleOrSingleton[T <: Tree](elems: List[T]) = 
      if (elems.length == 1)  
        elems.head
	  else
	    createTuple(elems)
	
	def primaryKeyDef(tableName: String)(columns: List[Column]) = {
	  def getColumn(c: Column) = Select(This(TypeName(tableName)), TermName(c.scalaName))
//	  val pks = if (columns.length == 1)  getColumn(columns.head)
//	  else //Apply(Select(Ident(TermName("scala.Tuple"+columns.length)), TermName("apply")), columns map getColumn)
//	    createTuple(columns map getColumn)
	  val pks = createTupleOrSingleton(columns map getColumn)
	  DefDef(Modifiers(), TermName("primaryKey"+tableName), List(), List(), TypeTree(), Apply(Select(This(TypeName(tableName)), TermName("primaryKey")), List(Literal(Constant("CONSTRAINT_PK_"+tableName.toUpperCase)),pks)))
	}
	
	def foreignKeyDef(tableName: String)(fk: Column.ForeignKey) = {
	  def getColumn(c: Column) = Select(This(TypeName(tableName)), TermName(c.scalaName))
	  def getColumnOfName(n: String)(c: Column) = Select(Ident(TermName(n)), TermName(c.scalaName))
//	  DefDef(Modifiers(), TermName("bFKTargetColumns"), List(), List(List(ValDef(Modifiers(PARAM), TermName("b"), SingletonTypeTree(Ident(TermName("B"))), EmptyTree))), TypeTree(), Apply(Select(Ident(scala.Tuple2), TermName("apply")), List(Select(Ident(TermName("b")), TermName("f1")), Select(Ident(TermName("b")), TermName("f2")))))
//	  DefDef(Modifiers(), TermName("bFK"), List(), List(), TypeTree(), Block(List(ValDef(Modifiers(ARTIFACT), TermName("x$25"), TypeTree(), Literal(Constant("b_fk"))), ValDef(Modifiers(ARTIFACT), TermName("x$26"), TypeTree(), Apply(Select(Ident(scala.Tuple2), TermName("apply")), List(Select(This(TypeName("A")), TermName("k1")), Select(This(TypeName("A")), TermName("k2"))))), ValDef(Modifiers(ARTIFACT), TermName("x$27"), TypeTree(), Ident(TermName("B"))), ValDef(Modifiers(ARTIFACT), TermName("x$28"), TypeTree(), Block(List(), Function(List(ValDef(Modifiers(PARAM | SYNTHETIC), TermName("b"), TypeTree(), EmptyTree)), Apply(Select(This(TypeName("A")), TermName("bFKTargetColumns")), List(Ident(TermName("b"))))))), ValDef(Modifiers(ARTIFACT), TermName("x$29"), TypeTree(), Select(Ident(scala.slick.lifted.ForeignKeyAction), TermName("Cascade"))), ValDef(Modifiers(ARTIFACT), TermName("x$30"), TypeTree(), Apply(Select(This(TypeName("A")), TermName("foreignKey$default$5")), List(Ident(TermName("x$25")), Ident(TermName("x$26")), Ident(TermName("x$27")))))), Apply(Apply(Apply(Select(This(TypeName("A")), TermName("foreignKey")), List(Ident(TermName("x$25")), Ident(TermName("x$26")), Ident(TermName("x$27")))), List(Ident(TermName("x$28")), Ident(TermName("x$30")), Ident(TermName("x$29")))), List(Apply(Select(Ident(scala.slick.lifted.Shape), TermName("unpackColumnBase")), List(Select(Ident(scala.Predef), TermName("conforms")))), Apply(Select(Ident(scala.slick.lifted.Shape), TermName("tuple2Shape")), List(Select(Ident(scala.slick.lifted.Shape), TermName("columnShape")), Select(Ident(scala.slick.lifted.Shape), TermName("columnShape"))))))))
//	  DefDef(Modifiers(), TermName("bFK"), List(), List(), TypeTree(), Apply(Apply(Select(This(TypeName("A")), TermName("foreignKey")), List(Literal(Constant("b_fk")), Apply(Select(Ident(scala.Tuple2), TermName("apply")), List(Select(This(TypeName("A")), TermName("k1")), Select(This(TypeName("A")), TermName("k2")))), Ident(TermName("B")))), List(Block(List(), Function(List(ValDef(Modifiers(PARAM | SYNTHETIC), TermName("b"), TypeTree(), EmptyTree)), Apply(Select(This(TypeName("A")), TermName("bFKTargetColumns")), List(Ident(TermName("b")))))), Select(Ident(scala.slick.lifted.ForeignKeyAction), TermName("NoAction")), Select(Ident(scala.slick.lifted.ForeignKeyAction), TermName("Cascade")))))
	  val fkName = Literal(Constant(fk.pkTable.table+"_fk"))
	  val fkFkColumns = fk.fields.map(_._2)
//	  val fkSourceColumns = 
//	    if (fkFkColumns.length == 1)  
//	      getColumn(fkFkColumns.head)
//	    else 
//	      createTuple(fkFkColumns map getColumn)
	  val fkSourceColumns = createTupleOrSingleton(fkFkColumns map getColumn)
	  val fkTargetTable = Ident(TermName(fk.pkTable.scalaName))
	  val fkPkColumns = fk.fields.map(_._1)
	  val SYNTHETIC = scala.reflect.internal.Flags.SYNTHETIC.asInstanceOf[Long].asInstanceOf[FlagSet]
	  val fkTargetColumns = Function(List(ValDef(Modifiers(PARAM | SYNTHETIC), TermName("x"), TypeTree(), EmptyTree)), createTupleOrSingleton(fkPkColumns map(getColumnOfName("x"))))
//	  val fkUpdateRule = Select(Ident(TermName("scala.slick.lifted.ForeignKeyAction")), TermName(fk.updateRule.toString))
//	  val fkDeleteRule = Select(Ident(TermName("scala.slick.lifted.ForeignKeyAction")), TermName(fk.deleteRule.toString))
	  val fkUpdateRule = Select(context.parse("scala.slick.lifted.ForeignKeyAction"), TermName(fk.updateRule.toString))
	  val fkDeleteRule = Select(context.parse("scala.slick.lifted.ForeignKeyAction"), TermName(fk.deleteRule.toString))
	  DefDef(Modifiers(), TermName("fk"+fk.pkTable.scalaName), List(), List(), TypeTree(), Apply(Apply(Select(This(TypeName(tableName)), TermName("foreignKey")), List(fkName, fkSourceColumns, fkTargetTable)), List(Block(List(), fkTargetColumns), fkUpdateRule, fkDeleteRule)))
	}
	
	def tableToModule(table: Schema, caseClass: ClassDef) = {
	  val columns = table.columns
	  val tableName = table.scalaName
	  val tableType = createClass("Table", Nil)
      val tableSuper = AppliedTypeTree(tableType, List(Ident(caseClass.name)))
      val superCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List(Literal(Constant(table.table))))
      val constructor = DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(superCall), Literal(Constant(()))))
      val fields = columns map columnToMethodDef(tableName)
	  val star = {
	    def getField(column: Column) = Select(This(TypeName(tableName)), TermName(column.scalaName))
	    val firstField = getField(columns.head)
	    val allFields = if (columns.length == 1) {
	      firstField
	    } else {
	      columns.tail.foldLeft[Tree](firstField)((prev, current) => {
	        val tilde = Select(prev, TermName("$tilde"))
	        Apply(tilde, List(getField(current)))
	      	}
	        )
	    }
	    val SYNTHETIC = scala.reflect.internal.Flags.SYNTHETIC.asInstanceOf[Long].asInstanceOf[FlagSet]
	    DefDef(Modifiers(), 
	        TermName("$times"), 
	        List(), 
	        List(), 
	        TypeTree(), 
	        Apply(
	            Select(allFields, TermName("$less$greater")), 
	            List(
	                Ident(caseClass.name.toTermName), 
	                Block(
	                    List(), 
	                    Function(
	                        List(ValDef(Modifiers(PARAM | SYNTHETIC), TermName("x$0"), TypeTree(), EmptyTree)), 
	                        Apply(Select(Ident(caseClass.name.toTermName), TermName("unapply")), List(Ident(TermName("x$0")))))
	                )
	            )
	        )
	     )
	  }
	  val pk = if (table.primaryKeys.isEmpty) None else Some(primaryKeyDef(tableName)(table.primaryKeys))
	  val fks = table.foreignKeys map foreignKeyDef(tableName)
      val methods = constructor :: (fields ++ (star :: (pk.toList ::: fks)))
      val tableModule = ModuleDef(NoMods, TermName(tableName), Template(List(tableSuper), emptyValDef, methods))
      tableModule
	}
	  
}