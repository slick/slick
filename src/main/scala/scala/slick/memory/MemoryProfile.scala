package scala.slick.memory

import scala.language.{implicitConversions, existentials}
import scala.collection.mutable.ArrayBuffer
import scala.slick.ast._
import scala.slick.compiler._
import scala.slick.lifted._
import scala.slick.profile.{RelationalMappingCompilerComponent, RelationalDriver, Capability, StandardParameterizedQueries, RelationalProfile}
import scala.slick.SlickException
import scala.slick.memory.QueryInterpreter.ProductValue
import TypeUtil.typeToTypeUtil

/** A profile and driver for interpreted queries on top of the in-memory database. */
trait MemoryProfile extends RelationalProfile with StandardParameterizedQueries { driver: MemoryDriver =>

  type Backend = HeapBackend
  type QueryExecutor[R] = QueryExecutorDef[R]
  type InsertInvoker[T] = InsertInvokerDef[T]
  type SchemaDescription = SchemaDescriptionDef
  type ColumnType[T] = ScalaType[T]
  type BaseColumnType[T] = ScalaType[T] with BaseTypedType[T]
  val backend: Backend = HeapBackend
  val Implicit: Implicits = new Implicits {}
  val simple: SimpleQL = new SimpleQL {}

  val compiler = QueryCompiler.standard + Phase.assignTypes
  lazy val queryCompiler = compiler + new MemoryCodeGen
  lazy val updateCompiler = compiler //+ new JdbcCodeGen[this.type](this)(_.buildUpdate)
  lazy val deleteCompiler = compiler //+ new JdbcCodeGen[this.type](this)(_.buildDelete)
  lazy val insertCompiler = QueryCompiler(new MemoryInsertCompiler)

  override protected def computeCapabilities = super.computeCapabilities ++ MemoryProfile.capabilities.all

  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R] = new QueryExecutorDef[R](tree, param)
  def createInsertInvoker[T](tree: scala.slick.ast.Node): InsertInvoker[T] = new InsertInvokerDef[T](tree)
  def compileParameterizedQuery[P,R](q: Query[_, R]) =
    new ParameterizedQuery[P, R](queryCompiler.run(Node(q)).tree)
  def buildSequenceSchemaDescription(seq: Sequence[_]): SchemaDescription = ???
  def buildTableSchemaDescription(table: Table[_]): SchemaDescription = new TableDDL(table)

  trait Implicits extends super.Implicits with ImplicitColumnTypes {
    implicit def ddlToDDLInvoker(d: SchemaDescription): DDLInvoker = d.asInstanceOf[DDLInvoker]
  }

  trait SimpleQL extends super.SimpleQL with Implicits

  trait ImplicitColumnTypes {
    implicit def booleanColumnType = ScalaType.booleanType
    implicit def bigDecimalColumnType = ScalaType.bigDecimalType
    implicit def byteColumnType = ScalaType.byteType
    implicit def charColumnType = ScalaType.charType
    implicit def doubleColumnType = ScalaType.doubleType
    implicit def floatColumnType = ScalaType.floatType
    implicit def intColumnType = ScalaType.intType
    implicit def longColumnType = ScalaType.longType
    implicit def shortColumnType = ScalaType.shortType
    implicit def stringColumnType = ScalaType.stringType
    implicit def unitColumnType = ScalaType.unitType
  }

  class QueryExecutorDef[R](tree: Node, param: Any) extends super.QueryExecutorDef[R] {
    def run(implicit session: Backend#Session): R = {
      val inter = new QueryInterpreter(session.database) {
        override def run(n: Node) = n match {
          case ResultSetMapping(gen, from, CompiledMapping(converter, tpe)) =>
            val fromV = run(from).asInstanceOf[TraversableOnce[Any]]
            val b = n.nodeType.asCollectionType.cons.canBuildFrom()
            b ++= fromV.map(v => converter.read(v.asInstanceOf[ProductValue]))
            b.result()
          case n => super.run(n)
        }
      }
      inter.run(tree).asInstanceOf[R]
    }
  }

  class InsertInvokerDef[T](tree: Node) extends super.InsertInvokerDef[T] {
    protected[this] val ResultSetMapping(_, Insert(_, table: TableNode, projection, _), CompiledMapping(converter, _)) = tree

    def += (value: T)(implicit session: Backend#Session) {
      val htable = session.database.getTable(table.tableName)
      val buf = htable.createInsertRow
      converter.set(value, buf)
      htable.append(buf)
    }

    def ++= (values: Iterable[T])(implicit session: Backend#Session): Unit =
      values.foreach(this += _)
  }

  abstract class DDL extends SchemaDescriptionDef with DDLInvoker { self =>
    def ++(other: SchemaDescription): SchemaDescription = {
      val d = Implicit.ddlToDDLInvoker(other)
      new DDL {
        def create(implicit session: Backend#Session) { self.create; d.create }
        def drop(implicit session: Backend#Session) { self.drop; d.drop }
      }
    }
  }

  class TableDDL(table: Table[_]) extends DDL {
    def create(implicit session: Backend#Session): Unit =
      session.database.createTable(table.tableName,
        table.create_*.map { fs => new HeapBackend.Column(fs, typeInfoFor(fs.tpe)) }.toIndexedSeq,
        table.indexes.toIndexedSeq, table.tableConstraints.toIndexedSeq)
    def drop(implicit session: Backend#Session): Unit =
      session.database.dropTable(table.tableName)
  }
}

object MemoryProfile {
  object capabilities {
    /** Supports all MemoryProfile features which do not have separate capability values */
    val other = Capability("memory.other")

    /** All MemoryProfile capabilities */
    val all = Set(other)
  }
}

trait MemoryDriver extends RelationalDriver with MemoryProfile with RelationalMappingCompilerComponent { driver =>

  /** The driver-specific representation of types */
  type TypeInfo = ScalaType[Any]
  def typeInfoFor(t: Type): TypeInfo = ((t match {
    case tmd: ScalaType[_] => tmd
    case StaticType.Boolean => ScalaType.booleanType
    case StaticType.Char => ScalaType.charType
    case StaticType.Int => ScalaType.intType
    case StaticType.Long => ScalaType.longType
    case StaticType.Null => ScalaType.nullType
    case StaticType.String => ScalaType.stringType
    case StaticType.Unit => ScalaType.unitType
    case o: OptionType => typeInfoFor(o.elementType).asInstanceOf[ScalaBaseType[_]].optionType
    case t => throw new SlickException("MemoryProfile has no TypeInfo for type "+t)
  }): ScalaType[_]).asInstanceOf[ScalaType[Any]]

  override val profile: MemoryProfile = this

  type RowReader = QueryInterpreter.ProductValue
  type RowWriter = ArrayBuffer[Any]
  type RowUpdater = ArrayBuffer[Any]

  class InsertMappingCompiler(insert: Insert) extends super.MappingCompiler {
    val Insert(_, table: Table[_], _, ProductNode(cols)) = insert
    val tableColumnIdxs = table.create_*.zipWithIndex.toMap

    def createColumnConverter(n: Node, path: Node, option: Boolean): ResultConverter = {
      val Select(_, ElementSymbol(ridx)) = path
      val Select(_, fs: FieldSymbol) = cols(ridx-1)
      val tidx = tableColumnIdxs(fs)
      new ResultConverter {
        def read(pr: RowReader) = ???
        def update(value: Any, pr: RowUpdater) = ???
        def set(value: Any, pp: RowWriter) = pp(tidx) = value
      }
    }
  }

  trait QueryMappingCompiler extends super.MappingCompiler {

    def createColumnConverter(n: Node, path: Node, option: Boolean): ResultConverter = {
      val Select(_, ElementSymbol(ridx)) = path
      val nullable = typeInfoFor(n.nodeType).nullable
      new ResultConverter {
        def read(pr: RowReader) = {
          val v = pr(ridx-1)
          if(!nullable && (v.asInstanceOf[AnyRef] eq null)) throw new SlickException("Read null value for non-nullable column")
          v
        }
        def update(value: Any, pr: RowUpdater) = ???
        def set(value: Any, pp: RowWriter) = ???
      }
    }
  }

  class MemoryInsertCompiler extends InsertCompiler {
    def createMapping(ins: Insert) =
      CompiledMapping(new InsertMappingCompiler(ins).compileMapping(ins.map), ins.map.nodeType)
  }

  class MemoryCodeGen extends CodeGen with QueryMappingCompiler {

    def apply(state: CompilerState): CompilerState = state.map(n => retype(apply(n, state)))

    def apply(node: Node, state: CompilerState): Node =
      ClientSideOp.mapResultSetMapping(node, keepType = true) { rsm =>
        val nmap = CompiledMapping(compileMapping(rsm.map), rsm.map.nodeType)
        rsm.copy(map = nmap).nodeTyped(rsm.nodeType)
      }

    def retype(n: Node): Node = {
      val n2 = n.nodeMapChildrenKeepType(retype)
      n2.nodeRebuildWithType(trType(n2.nodeType))
    }

    def trType(t: Type): Type = t match {
      case StructType(el) => StructType(el.map { case (s, t) => (s, trType(t)) })
      case ProductType(el) => ProductType(el.map(trType))
      case CollectionType(cons, el) => CollectionType(cons, trType(el))
      case t => typeInfoFor(t)
    }
  }
}

object MemoryDriver extends MemoryDriver
