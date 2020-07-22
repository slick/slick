package slick.lifted

import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.reflect.macros.blackbox

import slick.ast.{MappedScalaType, Node}

/** A value together with its Shape */
case class ShapedValue[T, U](value: T, shape: Shape[? <: FlatShapeLevel, T, U, ?]) extends Rep[U] {
  def encodeRef(path: Node): ShapedValue[T, U] = {
    val fv = shape.encodeRef(value, path).asInstanceOf[T]
    if(fv.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this else new ShapedValue(fv, shape)
  }
  def toNode = shape.toNode(value)
  def packedValue[R](implicit ev: Shape[? <: FlatShapeLevel, T, ?, R]): ShapedValue[R, U] = ShapedValue(shape.pack(value).asInstanceOf[R], shape.packedShape.asInstanceOf[Shape[FlatShapeLevel, R, U, ?]])
  def zip[T2, U2](s2: ShapedValue[T2, U2]) = new ShapedValue[(T, T2), (U, U2)]((value, s2.value), Shape.tuple2Shape(shape, s2.shape))
  def <>[R : ClassTag](f: U => R, g: R => Option[U]) = new MappedProjection[R, U](shape.toNode(value), MappedScalaType.Mapper(g.andThen(_.get).asInstanceOf[Any => Any], f.asInstanceOf[Any => Any], None), implicitly[ClassTag[R]])
  @inline def shaped: ShapedValue[T, U] = this

  def mapTo[R <: Product & Serializable](implicit rCT: ClassTag[R]): MappedProjection[R, U] = macro ShapedValue.mapToImpl[R, U]
  override def toString = s"ShapedValue($value, $shape)"
}

object ShapedValue {
  def mapToImpl[R <: Product & Serializable, U](c: blackbox.Context {type PrefixType = ShapedValue[?, U]})
                                               (rCT: c.Expr[ClassTag[R]])
                                               (implicit rTag: c.WeakTypeTag[R], uTag: c.WeakTypeTag[U]): c.Tree = {
    import c.universe.*
    val rSym = symbolOf[R]
    if (!rSym.isClass || !rSym.asClass.isCaseClass)
      c.abort(c.enclosingPosition, s"${rSym.fullName} must be a case class")
    val rModule = rSym.companion match {
      case NoSymbol => q"${rSym.name.toTermName}" // This can happen for case classes defined inside of methods
      case s        => q"$s"
    }
    val rHasTupled = rSym.companion match {
      case NoSymbol => true
      case s        => s.info.member(TermName("tupled")) != NoSymbol
    }
    val fields =
      rTag.tpe.decls
        .collect {
          case s: TermSymbol if s.isVal && s.isCaseAccessor =>
            (TermName(s.name.toString.trim), s.typeSignature, TermName(c.freshName()))
        }
        .toIndexedSeq
    val (f, g) = if(uTag.tpe <:< c.typeOf[slick.collection.heterogeneous.HList]) { // Map from HList
      val rTypeAsHList = fields.foldRight[Tree](tq"_root_.slick.collection.heterogeneous.HNil.type") {
        case ((_, t, _), z) => tq"_root_.slick.collection.heterogeneous.HCons[$t, $z]"
      }
      val pat = fields.foldRight[Tree](pq"_root_.slick.collection.heterogeneous.HNil") {
        case ((_, _, n), z) => pq"_root_.slick.collection.heterogeneous.HCons($n, $z)"
      }
      val cons = fields.foldRight[Tree](q"_root_.slick.collection.heterogeneous.HNil") {
        case ((n, _, _), z) => q"v.$n :: $z"
      }
      (q"({ case $pat => new $rTag(..${fields.map(_._3)}) } : ($rTypeAsHList => $rTag)): ($uTag => $rTag)",
        q"{ case v => $cons }: ($rTag => $uTag)")
    } else if (fields.length == 1) { // Map from single value
      (q"($rModule.apply _) : ($uTag => $rTag)",
        q"(($rModule.unapply _) : $rTag => Option[$uTag]).andThen(_.get)")
    } else if (rHasTupled) { // Map from tuple
      (q"($rModule.tupled) : ($uTag => $rTag)",
        q"(($rModule.unapply _) : $rTag => Option[$uTag]).andThen(_.get)")
    } else { // Map from tuple with tupled apply
      (q"(($rModule.apply _).tupled) : ($uTag => $rTag)",
        q"(($rModule.unapply _) : $rTag => Option[$uTag]).andThen(_.get)")
    }

    val fpName = Constant("Fast Path of (" + fields.map(_._2).mkString(", ") + ").mapTo[" + rTag.tpe + "]")
    val fpChildren = fields.map { case (_, t, n) => q"val $n = next[$t]" }
    val fpReadChildren = fields.map { case (_, _, n) => q"$n.read(r)" }
    val fpSetChildren = fields.map { case (fn, _, n) => q"$n.set(value.$fn, pp, index)" }
    val fpUpdateChildren = fields.map { case (fn, _, n) => q"$n.update(value.$fn, pr)" }

    q"""
      val ff = $f.asInstanceOf[_root_.scala.Any => _root_.scala.Any] // Resolving f first creates more useful type errors
      val gg = $g.asInstanceOf[_root_.scala.Any => _root_.scala.Any]
      val fpMatch: (_root_.scala.Any => _root_.scala.Any) = {
        case tm @ _root_.slick.relational.TypeMappingResultConverter(_: _root_.slick.relational.ProductResultConverter[_, _, _, _], _, _) =>
          new _root_.slick.relational.SimpleFastPathResultConverter[_root_.scala.Any, _root_.scala.Any, _root_.scala.Any, $rTag](tm.asInstanceOf[_root_.slick.relational.TypeMappingResultConverter[_root_.scala.Any, _root_.scala.Any, _root_.scala.Any, $rTag, _]]) {
            ..$fpChildren
            override def read(r: Reader): $rTag = new $rTag(..$fpReadChildren)
            override def set(value: $rTag, pp: Writer, index: Int): _root_.scala.Unit = {..$fpSetChildren}
            override def update(value: $rTag, pr: Updater): _root_.scala.Unit = {..$fpUpdateChildren}
            override def getDumpInfo = super.getDumpInfo.copy(name = $fpName)
          }
        case tm => tm
      }
      new _root_.slick.lifted.MappedProjection[$rTag, $uTag](${c.prefix}.toNode,
        _root_.slick.ast.MappedScalaType.Mapper(gg, ff, _root_.scala.Some(fpMatch)), $rCT)
    """
  }

  type Unconst[P, P2] = P
}
