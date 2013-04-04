package scala.slick.config

import scala.slick.schema.QualifiedName
import scala.slick.typeproviders.TypeMapper
import scala.slick.typeproviders.TypeExtractor
//import scala.slick.typeproviders.types.TypeExtractorTyped
import scala.reflect.api.Universe

object CustomTyping extends TypeMapper {
  implicit def implicit_a = 2
  implicit def implicit_b = ""
  implicit val implicit_c = 24.1
  type SimpleA = Tuple2[Int, String]
  //  object Supplier extends TypeExtractor[Supplier, Supplier] {
  class SimpleAExtractor extends TypeExtractor[SimpleA, SimpleA] {
    //  class SupplierExtractor extends TypeExtractor {
    //    type Elem = Supplier
    //    type TupleElem = Supplier
    override def unapply(s: SimpleA): Option[SimpleA] = Tuple2.unapply(s)
    //    override def apply(_1: Int, _2: String, _3: String, _4: String, _5: String, _6: String) = Tuple6.apply(_1, _2, _3, _4, _5, _6)
    def apply(s: SimpleA): SimpleA = s
    //    override def apply(varargs: AnyRef*): Supplier =
    //      varargs.toList match {
    //        case List(_1, _2, _3, _4, _5, _6) => apply(_1, _2, _3, _4, _5, _6)
    //        case _ => throw new scala.slick.SlickException("apply with incorrect number of params")
    //      }
  }
  //  val Supplier = Tuple6

  override def tableType(universe: Universe)(name: QualifiedName): Option[universe.Type] = name.lastPart match {
    case "SIMPLE_AS" => Some(universe.typeOf[SimpleA])
    case _ => super.tableType(universe)(name)
  }

  override def tableExtractor(universe: Universe)(name: QualifiedName): Option[universe.Type] = name.lastPart match {
    //  override def tableExtractor(name: QualifiedName): Option[TypeExtractor] = name.lastPart match {
    //  override def tableExtractor(name: QualifiedName): Option[TypeExtractor[_, _]] = name.lastPart match {
    //  override def tableExtractor(name: QualifiedName): Option[AnyRef] = name.lastPart match {
    case "SIMPLE_AS" => Some(universe.typeOf[SimpleAExtractor])
    case _ => super.tableExtractor(universe)(name)
  }

}
