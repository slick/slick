package scala.slick.memory

import scala.language.{implicitConversions, existentials}
import scala.collection.mutable.ArrayBuffer
import scala.slick.ast._
import scala.slick.compiler._
import scala.slick.profile.Capability
import TypeUtil.typeToTypeUtil

/** A profile and driver for interpreted queries on top of the in-memory database. */
trait MemoryProfile extends MemoryQueryingProfile { driver: MemoryDriver =>

  type SchemaDescription = SchemaDescriptionDef
  type InsertInvoker[T] = InsertInvokerDef[T]
  type QueryExecutor[R] = QueryExecutorDef[R]
  type Backend = HeapBackend
  val backend: Backend = HeapBackend
  val simple: SimpleQL = new SimpleQL {}
  val Implicit: Implicits = simple

  val compiler = QueryCompiler.standard
  lazy val queryCompiler = compiler + new MemoryCodeGen
  lazy val updateCompiler = compiler
  lazy val deleteCompiler = compiler
  lazy val insertCompiler = QueryCompiler(Phase.inline, Phase.assignUniqueSymbols, new MemoryInsertCompiler)

  override protected def computeCapabilities = super.computeCapabilities ++ MemoryProfile.capabilities.all

  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R] = new QueryExecutorDef[R](tree, param)
  def createInsertInvoker[T](tree: scala.slick.ast.Node): InsertInvoker[T] = new InsertInvokerDef[T](tree)
  def createDDLInvoker(sd: SchemaDescription): DDLInvoker = ???
  def buildSequenceSchemaDescription(seq: Sequence[_]): SchemaDescription = ???
  def buildTableSchemaDescription(table: Table[_]): SchemaDescription = new TableDDL(table)

  trait Implicits extends super.Implicits {
    implicit def ddlToDDLInvoker(d: SchemaDescription): DDLInvoker = d.asInstanceOf[DDLInvoker]
  }

  trait SimpleQL extends super.SimpleQL with Implicits

  class QueryExecutorDef[R](tree: Node, param: Any) extends super.QueryExecutorDef[R] {
    def run(implicit session: Backend#Session): R = {
      val inter = new QueryInterpreter(session.database, param) {
        override def run(n: Node) = n match {
          case ResultSetMapping(gen, from, CompiledMapping(converter, tpe)) =>
            val fromV = run(from).asInstanceOf[TraversableOnce[Any]]
            val b = n.nodeType.asCollectionType.cons.createErasedBuilder
            b ++= fromV.map(v => converter.read(v.asInstanceOf[QueryInterpreter.ProductValue]))
            b.result()
          case n => super.run(n)
        }
      }
      inter.run(tree).asInstanceOf[R]
    }
  }

  class InsertInvokerDef[T](tree: Node) extends super.InsertInvokerDef[T] {
    protected[this] val ResultSetMapping(_, Insert(_, table: TableNode, projection, _), CompiledMapping(converter, _)) = tree

    type SingleInsertResult = Unit
    type MultiInsertResult = Unit

    def += (value: T)(implicit session: Backend#Session) {
      val htable = session.database.getTable(table.tableName)
      val buf = htable.createInsertRow
      converter.set(value, buf, false)
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

trait MemoryDriver extends MemoryQueryingDriver with MemoryProfile { driver =>

  override val profile: MemoryProfile = this

  type RowWriter = ArrayBuffer[Any]

  class InsertMappingCompiler(insert: Insert) extends super.MappingCompiler {
    val Insert(_, table: TableNode, _, ProductNode(cols)) = insert
    val tableColumnIdxs = table.driverTable.asInstanceOf[Table[_]].create_*.zipWithIndex.toMap

    def createColumnConverter(n: Node, path: Node, option: Boolean, column: Option[FieldSymbol]): ResultConverter = {
      val fs = column.get
      val tidx = tableColumnIdxs(fs)
      val autoInc = fs.options.contains(ColumnOption.AutoInc)
      new ResultConverter {
        def read(pr: RowReader) = ???
        def update(value: Any, pr: RowUpdater) = ???
        def set(value: Any, pp: RowWriter, forced: Boolean) =
          if(forced || !autoInc) pp(tidx) = value
      }
    }
  }

  class MemoryInsertCompiler extends InsertCompiler {
    def createMapping(ins: Insert) =
      CompiledMapping(new InsertMappingCompiler(ins).compileMapping(ins.map), ins.map.nodeType)
  }
}

object MemoryDriver extends MemoryDriver
