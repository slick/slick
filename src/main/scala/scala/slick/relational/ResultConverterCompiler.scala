package scala.slick.relational

import scala.language.existentials
import scala.slick.ast._
import scala.slick.SlickException
import scala.slick.util.SlickLogger
import org.slf4j.LoggerFactory

/** Create a ResultConverter for parameters and result sets. Subclasses have
  * to provide profile-specific createColumnConverter implementations. */
trait ResultConverterCompiler[Domain <: ResultConverterDomain] {

  protected[this] lazy val resultConverterLogger = new SlickLogger(LoggerFactory.getLogger(classOf[ResultConverterCompiler[_]]))

  def compile(n: Node): ResultConverter[Domain, _] = n match {
    case InsertColumn(p @ Path(_), fs) => createColumnConverter(n, p, false, Some(fs))
    case OptionApply(InsertColumn(p @ Path(_), fs)) => createColumnConverter(n, p, true, Some(fs))
    case p @ Path(_) => createColumnConverter(n, p, false, None)
    case OptionApply(p @ Path(_)) => createColumnConverter(n, p, true, None)
    case ProductNode(ch) =>
      new ProductResultConverter(ch.map(n => compile(n))(collection.breakOut): _*)
    case GetOrElse(ch, default) =>
      createGetOrElseResultConverter(compile(ch).asInstanceOf[ResultConverter[Domain, Option[Any]]], default)
    case TypeMapping(ch, mapper, _) =>
      createTypeMappingResultConverter(compile(ch).asInstanceOf[ResultConverter[Domain, Any]], mapper)
    case n =>
      throw new SlickException("Unexpected node in ResultSetMapping: "+n)
  }

  def createGetOrElseResultConverter[T](rc: ResultConverter[Domain, Option[T]], default: () => T): ResultConverter[Domain, T] =
    new GetOrElseResultConverter[Domain, T](rc, default)

  def createTypeMappingResultConverter(rc: ResultConverter[Domain, Any], mapper: MappedScalaType.Mapper): ResultConverter[Domain, Any] =
    new TypeMappingResultConverter(rc, mapper.toBase, mapper.toMapped)

  def createColumnConverter(n: Node, path: Node, optionApply: Boolean, column: Option[FieldSymbol]): ResultConverter[Domain, _]

  def compileMapping(n: Node): CompiledMapping = {
    val rc = compile(n)
    if(resultConverterLogger.isDebugEnabled)
      resultConverterLogger.debug("Compiled ResultConverter:\n"+ResultConverter.getDump(rc, prefix = "  "))
    CompiledMapping(rc, n.nodeType)
  }
}

/** A node that wraps a ResultConverter */
final case class CompiledMapping(converter: ResultConverter[_ <: ResultConverterDomain, _], tpe: Type) extends NullaryNode with TypedNode {
  type Self = CompiledMapping
  def nodeRebuild = copy()
  override def toString = "CompiledMapping"
}
