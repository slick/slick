package scala.slick.jdbc

import scala.slick.compiler.{CompilerState, CodeGen}
import scala.slick.ast.{NullaryNode, GetOrElse, OptionApply, TypeMapping, UnaryNode, ProductNode, Path, TypedNode, Type, CompiledStatement, ClientSideOp, Node}
import scala.slick.driver.JdbcDriver
import scala.slick.util.{TupleSupport, SQLBuilder}
import scala.slick.SlickException

/** Code generator phase for JdbcProfile-based drivers. */
class JdbcCodeGen[Driver <: JdbcDriver](val driver: Driver)(f: Driver#QueryBuilder => SQLBuilder.Result) extends CodeGen {

  def apply(state: CompilerState): CompilerState = state.map(n => apply(n, state))

  def apply(node: Node, state: CompilerState): Node =
    ClientSideOp.mapResultSetMapping(node, keepType = true) { rsm =>
      val sbr = f(driver.createQueryBuilder(rsm.from, state))
      val nfrom = CompiledStatement(sbr.sql, sbr, rsm.from.nodeType)
      val nmap = CompiledMapping(compileMapping(rsm.map), rsm.map.nodeType)
      rsm.copy(from = nfrom, map = nmap).nodeTyped(rsm.nodeType)
    }

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
}

final class ColumnResultConverter(ti: JdbcType[Any], path: Node) extends ResultConverter {
  def read(pr: PositionedResult) = ti.nextValueOrElse(
    if(ti.nullable) ti.zero
    else throw new SlickException("Read NULL value for ResultSet column "+path),
    pr
  )
  def update(value: Any, pr: PositionedResult) = ti.updateValue(value, pr)
}

final class OptionApplyColumnResultConverter(ti: JdbcType[Any]) extends ResultConverter {
  def read(pr: PositionedResult) = ti.nextValue(pr)
  def update(value: Any, pr: PositionedResult) = ti.updateValue(value, pr)
}

final class ProductResultConverter(children: IndexedSeq[ResultConverter]) extends ResultConverter {
  def read(pr: PositionedResult) = TupleSupport.buildTuple(children.map(_.read(pr)))
  def update(value: Any, pr: PositionedResult) =
    children.iterator.zip(value.asInstanceOf[Product].productIterator).foreach { case (ch, v) =>
      ch.update(v, pr)
    }
}

final class GetOrElseResultConverter(child: ResultConverter, default: () => Any) extends ResultConverter {
  def read(pr: PositionedResult) = child.read(pr).asInstanceOf[Option[Any]].getOrElse(default())
  def update(value: Any, pr: PositionedResult) = child.update(Some(value), pr)
}

final class TypeMappingResultConverter(child: ResultConverter, toBase: Any => Any, toMapped: Any => Any) extends ResultConverter {
  def read(pr: PositionedResult) = toMapped(child.read(pr))
  def update(value: Any, pr: PositionedResult) = child.update(toBase(value), pr)
}
