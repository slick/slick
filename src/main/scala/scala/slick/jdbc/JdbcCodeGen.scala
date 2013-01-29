package scala.slick.jdbc

import scala.slick.compiler.{CompilerState, CodeGen}
import scala.slick.ast.{GetOrElse, OptionApply, TypeMapping, UnaryNode, ProductNode, Path, TypedNode, Type, CompiledStatement, ClientSideOp, Node}
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
      val nmap = compileMapping(rsm.map)
      rsm.copy(from = nfrom, map = nmap).nodeTyped(rsm.nodeType)
    }

  /* TODO: JdbcTypeReader/Writer isn't the right interface -- it assumes that
   * all columns will be read in order. We should not limit it in this way. */
  def compileMapping(n: Node): PositionedResultReader = n match {
    case p @ Path(_) =>
      // TODO: Extract the correct index instead of assuming sequential access
      val ti = driver.typeInfoFor(n.nodeType)
      PositionedResultReader(n, { pr =>
        ti.nextValueOrElse(
          if(ti.nullable) ti.zero else throw new SlickException("Read NULL value for ResultSet column "+p),
            pr)
      }, n.nodeType)
    case OptionApply(Path(_)) =>
      // TODO: Extract the correct index instead of assuming sequential access
      PositionedResultReader(n, driver.typeInfoFor(n.nodeType).nextValue _, n.nodeType)
    case ProductNode(ch) =>
      val readers: IndexedSeq[PositionedResult => Any] =
        ch.map(n => compileMapping(n).read)(collection.breakOut)
      PositionedResultReader(n, { pr =>
        TupleSupport.buildTuple(readers.map(_.apply(pr)))
      }, n.nodeType)
    case GetOrElse(ch, default) =>
      val chreader = compileMapping(ch).read
      PositionedResultReader(n, { pr => chreader(pr).asInstanceOf[Option[Any]].getOrElse(default()) }, n.nodeType)
    case TypeMapping(ch, _, _, toMapped) =>
      val chreader = compileMapping(ch).read
      PositionedResultReader(n, { pr => toMapped(chreader(pr)) }, n.nodeType)
    case n =>
      throw new SlickException("Unexpected node in ResultSetMapping: "+n)
  }
}

/** A node that wraps a function for reading a row from a PositionedResult */
final case class PositionedResultReader(child: Node, read: PositionedResult => Any, tpe: Type) extends UnaryNode with TypedNode {
  type Self = PositionedResultReader
  def nodeRebuild(ch: Node) = copy(child = ch)
  override def toString = "PositionedResultReader"
}

/** A node that wraps the execution of an SQL statement */
final case class ExecuteStatement(child: Node, call: Any => PositionedResult, tpe: Type) extends UnaryNode with TypedNode {
  type Self = ExecuteStatement
  def nodeRebuild(ch: Node) = copy(child = ch)
  override def toString = "PositionedResultReader"
}
