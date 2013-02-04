package scala.slick.jdbc

import scala.slick.compiler.{Phase, CompilerState, CodeGen}
import scala.slick.ast._
import scala.slick.driver.JdbcDriver
import scala.slick.util.{TupleSupport, SQLBuilder}
import scala.slick.SlickException
import scala.collection.mutable.ArrayBuffer

/** Code generator phase for JdbcProfile-based drivers. */
class JdbcCodeGen[Driver <: JdbcDriver](val driver: Driver)(f: Driver#QueryBuilder => SQLBuilder.Result) extends CodeGen with MappingCompiler {

  def apply(state: CompilerState): CompilerState = state.map(n => apply(n, state))

  def apply(node: Node, state: CompilerState): Node =
    ClientSideOp.mapResultSetMapping(node, keepType = true) { rsm =>
      val sbr = f(driver.createQueryBuilder(rsm.from, state))
      val nfrom = CompiledStatement(sbr.sql, sbr, rsm.from.nodeType)
      val nmap = CompiledMapping(compileMapping(rsm.map), rsm.map.nodeType)
      rsm.copy(from = nfrom, map = nmap).nodeTyped(rsm.nodeType)
    }
}

trait MappingCompiler {
  def driver: JdbcDriver

  def compileMapping(n: Node): ResultConverter = n match {
    case Path(_) =>
      new ColumnResultConverter(driver.typeInfoFor(n.nodeType), n)
    case OptionApply(Path(_)) =>
      new OptionApplyColumnResultConverter(driver.typeInfoFor(n.nodeType))
    case ProductNode(ch) =>
      new ProductResultConverter(ch.map(n => compileMapping(n))(collection.breakOut))
    case GetOrElse(ch, default) =>
      new GetOrElseResultConverter(compileMapping(ch), default)
    case TypeMapping(ch, _, toBase, toMapped) =>
      new TypeMappingResultConverter(compileMapping(ch), toBase, toMapped)
    case n =>
      throw new SlickException("Unexpected node in ResultSetMapping: "+n)
  }
}

/** A node that wraps a ResultConverter */
final case class CompiledMapping(converter: ResultConverter, tpe: Type) extends NullaryNode with TypedNode {
  type Self = CompiledMapping
  def nodeRebuild = copy()
  override def toString = "CompiledMapping"
}

/** A node that wraps the execution of an SQL statement */
final case class ExecuteStatement(child: Node, call: Any => PositionedResult, tpe: Type) extends UnaryNode with TypedNode {
  type Self = ExecuteStatement
  def nodeRebuild(ch: Node) = copy(child = ch)
  override def toString = "PositionedResultReader"
}

trait ResultConverter {
  /* TODO: PositionedResult isn't the right interface -- it assumes that
   * all columns will be read and updated in order. We should not limit it in
   * this way. */
  def read(pr: PositionedResult): Any
  def update(value: Any, pr: PositionedResult): Unit
  def set(value: Any, pp: PositionedParameters): Unit
}

final class ColumnResultConverter(ti: JdbcType[Any], path: Node) extends ResultConverter {
  def read(pr: PositionedResult) = ti.nextValueOrElse(
    if(ti.nullable) ti.zero
    else throw new SlickException("Read NULL value for ResultSet column "+path),
    pr
  )
  def update(value: Any, pr: PositionedResult) = ti.updateValue(value, pr)
  def set(value: Any, pp: PositionedParameters) = ti.setValue(value, pp)
}

final class OptionApplyColumnResultConverter(ti: JdbcType[Any]) extends ResultConverter {
  def read(pr: PositionedResult) = ti.nextValue(pr)
  def update(value: Any, pr: PositionedResult) = ti.updateValue(value, pr)
  def set(value: Any, pp: PositionedParameters) = ti.setValue(value, pp)
}

final class ProductResultConverter(children: IndexedSeq[ResultConverter]) extends ResultConverter {
  def read(pr: PositionedResult) = TupleSupport.buildTuple(children.map(_.read(pr)))
  def update(value: Any, pr: PositionedResult) =
    children.iterator.zip(value.asInstanceOf[Product].productIterator).foreach { case (ch, v) =>
      ch.update(v, pr)
    }
  def set(value: Any, pp: PositionedParameters) =
    children.iterator.zip(value.asInstanceOf[Product].productIterator).foreach { case (ch, v) =>
      ch.set(v, pp)
    }
}

final class GetOrElseResultConverter(child: ResultConverter, default: () => Any) extends ResultConverter {
  def read(pr: PositionedResult) = child.read(pr).asInstanceOf[Option[Any]].getOrElse(default())
  def update(value: Any, pr: PositionedResult) = child.update(Some(value), pr)
  def set(value: Any, pp: PositionedParameters) = child.set(Some(value), pp)
}

final class TypeMappingResultConverter(child: ResultConverter, toBase: Any => Any, toMapped: Any => Any) extends ResultConverter {
  def read(pr: PositionedResult) = toMapped(child.read(pr))
  def update(value: Any, pr: PositionedResult) = child.update(toBase(value), pr)
  def set(value: Any, pp: PositionedParameters) = child.set(toBase(value), pp)
}

/** A custom compiler for INSERT statements. We could reuse the standard
  * phases with a minor modification instead, but this is much faster. */
class CompileInsert(val driver: JdbcDriver) extends Phase with MappingCompiler {
  val name = "compileInsert"

  def apply(state: CompilerState) = state.map { tree =>
    val gen, rgen = new AnonSymbol
    val tref = Ref(gen)
    val rref = Ref(rgen)

    var table: TableNode = null
    val cols = new ArrayBuffer[Select]

    def tr(n: Node): Node = n match {
      case _: OptionApply | _: GetOrElse | _: ProductNode | _: TypeMapping => n.nodeMapChildrenKeepType(tr)
      case t:TableNode => tr(Node(t.nodeShaped_*.value))
      case sel @ Select(Ref(IntrinsicSymbol(t: TableNode)), fs: FieldSymbol) =>
        if(table eq null) table = t
        else if(table ne t) throw new SlickException("Inserts must all be to the same table")
        cols += Select(tref, fs).nodeTyped(sel.nodeType)
        Select(rref, ElementSymbol(cols.size)).nodeTyped(sel.nodeType)
      case _ => throw new SlickException("Cannot use node "+n+" in INSERT statement")
    }
    val tree2 = tr(tree)
    if(table eq null) throw new SlickException("No table to insert into")
    val ins = Insert(gen, table, tree2, ProductNode(cols)).nodeWithComputedType(SymbolScope.empty, retype = false)
    logger.debug("Insert node:", ins)

    val nmap = CompiledMapping(compileMapping(ins.map), ins.map.nodeType)
    ResultSetMapping(rgen, ins, nmap).nodeTyped(
      CollectionType(CollectionTypeConstructor.default, ins.nodeType))
  }
}

final case class Insert(generator: Symbol, table: Node, map: Node, linear: Node) extends Node with DefNode {
  type Self = Insert
  def left = table
  def right = map
  override def nodeChildNames = Vector("table", "map", "linear")
  def nodeChildren = Vector(table, map, linear)
  def nodeGenerators = Vector((generator, table))
  def nodeRebuild(ch: IndexedSeq[Node]) = copy(table = ch(0), map = ch(1), linear = ch(2))
  def nodeRebuildWithGenerators(gen: IndexedSeq[Symbol]) = copy(generator = gen(0))
  def nodeWithComputedType(scope: SymbolScope, retype: Boolean): Self = if(nodeHasType && !retype) this else {
    val t2 = table.nodeWithComputedType(scope, retype)
    val m2 = map.nodeWithComputedType(scope + (generator -> t2.nodeType), retype)
    val newType = m2.nodeType
    if((t2 eq table) && (m2 eq map) && newType == nodeType) this
    else copy(table = t2, map = m2).nodeTyped(newType)
  }
}
