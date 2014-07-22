package scala.slick.relational

import scala.language.existentials
import scala.slick.ast._
import scala.slick.SlickException
import scala.slick.util.SlickLogger
import org.slf4j.LoggerFactory

/** Create a ResultConverter for parameters and result sets. Subclasses have
  * to provide profile-specific createColumnConverter implementations. */
trait ResultConverterCompiler[Domain <: ResultConverterDomain] {

  def compile(n: Node): ResultConverter[Domain, _] = n match {
    case InsertColumn(paths, fs, _) =>
      val pathConvs = paths.map { case Select(_, ElementSymbol(idx)) => createColumnConverter(n, idx, Some(fs)) }
      if(pathConvs.length == 1) pathConvs.head else CompoundResultConverter(1, pathConvs: _*)
    case OptionApply(InsertColumn(paths, fs, _)) =>
      val pathConvs = paths.map { case Select(_, ElementSymbol(idx)) => createColumnConverter(n, idx, Some(fs)) }
      if(pathConvs.length == 1) pathConvs.head else CompoundResultConverter(1, pathConvs: _*)
    case Select(_, ElementSymbol(idx)) => createColumnConverter(n, idx, None)
    case cast @ Library.SilentCast(sel @ Select(_, ElementSymbol(idx))) =>
      createColumnConverter(sel.nodeTypedOrCopy(cast.nodeType), idx, None)
    case OptionApply(Select(_, ElementSymbol(idx))) => createColumnConverter(n, idx, None)
    case ProductNode(ch) =>
      if(ch.isEmpty) new UnitResultConverter
      else new ProductResultConverter(ch.map(n => compile(n))(collection.breakOut): _*)
    case GetOrElse(ch, default) =>
      createGetOrElseResultConverter(compile(ch).asInstanceOf[ResultConverter[Domain, Option[Any]]], default)
    case TypeMapping(ch, mapper, _) =>
      createTypeMappingResultConverter(compile(ch).asInstanceOf[ResultConverter[Domain, Any]], mapper)
    case RebuildOption(disc, data) =>
      val discConv = createGetOrElseResultConverter(compile(disc).asInstanceOf[ResultConverter[Domain, Option[Int]]], () => 0)
      val dataConv = compile(data).asInstanceOf[ResultConverter[Domain, Any]]
      createOptionRebuildingConverter(discConv, dataConv)
    case n =>
      throw new SlickException("Unexpected node in ResultSetMapping: "+n)
  }

  def createGetOrElseResultConverter[T](rc: ResultConverter[Domain, Option[T]], default: () => T): ResultConverter[Domain, T] =
    new GetOrElseResultConverter[Domain, T](rc, default)

  def createTypeMappingResultConverter(rc: ResultConverter[Domain, Any], mapper: MappedScalaType.Mapper): ResultConverter[Domain, Any] =
    new TypeMappingResultConverter(rc, mapper.toBase, mapper.toMapped)

  def createOptionRebuildingConverter(discriminator: ResultConverter[Domain, Int], data: ResultConverter[Domain, Any]): ResultConverter[Domain, Option[Any]] =
    new OptionRebuildingResultConverter(discriminator, data)

  def createColumnConverter(n: Node, idx: Int, column: Option[FieldSymbol]): ResultConverter[Domain, _]

  def compileMapping(n: Node): CompiledMapping = {
    val rc = compile(n)
    ResultConverterCompiler.logger.debug("Compiled ResultConverter", rc)
    CompiledMapping(rc, n.nodeType)
  }
}

object ResultConverterCompiler {
  protected lazy val logger = new SlickLogger(LoggerFactory.getLogger(classOf[ResultConverterCompiler[_]]))
}

/** A node that wraps a ResultConverter */
final case class CompiledMapping(converter: ResultConverter[_ <: ResultConverterDomain, _], tpe: Type) extends NullaryNode with TypedNode {
  type Self = CompiledMapping
  def nodeRebuild = copy()
  override def getDumpInfo = {
    val di = super.getDumpInfo
    di.copy(mainInfo = "", children = di.children ++ Vector(("converter", converter)))
  }
}
