package slick.lifted

import slick.ast.{MappedScalaType, Node}
import slick.collection.heterogeneous._

import scala.compiletime._
import scala.deriving.{ArrayProduct, Mirror}
import scala.quoted._
import scala.reflect.ClassTag

/** A value together with its Shape */
case class ShapedValue[T, U](value: T, shape: Shape[_ <: FlatShapeLevel, T, U, _]) extends Rep[U] {
  def encodeRef(path: Node): ShapedValue[T, U] = {
    val fv = shape.encodeRef(value, path).asInstanceOf[T]
    if(fv.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this else new ShapedValue(fv, shape)
  }
  def toNode = shape.toNode(value)
  def packedValue[R](implicit ev: Shape[_ <: FlatShapeLevel, T, _, R]): ShapedValue[R, U] = ShapedValue(shape.pack(value).asInstanceOf[R], shape.packedShape.asInstanceOf[Shape[FlatShapeLevel, R, U, _]])
  def zip[T2, U2](s2: ShapedValue[T2, U2]) = new ShapedValue[(T, T2), (U, U2)]((value, s2.value), Shape.tuple2Shape(shape, s2.shape))
  def <>[R : ClassTag, T](f: (U => R), g: (R => T))(implicit tt: ToTuple[T, U]) =
    new MappedProjection[R, U](shape.toNode(value), MappedScalaType.Mapper(g.andThen(tt.apply).andThen(_.get).asInstanceOf[Any => Any], f.asInstanceOf[Any => Any], None), implicitly[ClassTag[R]])
  @inline def shaped: ShapedValue[T, U] = this

  inline def mapTo[R]: MappedProjection[R, U] = ${ ShapedValue.mapToExpr[R, T, U]('{this}) }
  override def toString = s"ShapedValue($value, $shape)"
}

object ShapedValue {
  def mapToExpr[R : Type, T : Type, U : Type](sv: Expr[ShapedValue[T, U]])(implicit qctx: QuoteContext): Expr[MappedProjection[R, U]] = {
    val rtpe = summon[Type[R]]
    val utpe = summon[Type[U]]
    val rct = Expr.summon[ClassTag[R]].getOrElse(report.throwError(s"No ClassTag available for ${rtpe}"))
    val rpm = Expr.summon[Mirror.ProductOf[R]].getOrElse(report.throwError(s"${rtpe.show} is not a product type"))

    def decomposeTuple(tpe: Type[_ <: Tuple]): List[Type[_]] = tpe match {
      case '[ $t *: $ts ] => t :: decomposeTuple(ts)
      case '[ EmptyTuple ] => Nil
    }

    def decomposeHList(tpe: Type[_ <: HList]): List[Type[_]] = tpe match {
      case '[ HCons[$t, $ts] ] => t :: decomposeHList(ts)
      case '[ HNil.type ] => Nil
    }

    val targetElemTpes = rpm match {
      case '{ $m: Mirror.ProductOf[R] { type MirroredElemTypes = $ts }} =>
        decomposeTuple(ts.asInstanceOf[Type[_ <: Tuple]])
    }

    val (f, g, elemTpes) = Expr.summon[Mirror.ProductOf[U]] match {
      case Some(upm @ '{ $m: Mirror.ProductOf[U] { type MirroredElemTypes = $elementTypes }}) =>
        val elemTpes = decomposeTuple(elementTypes.asInstanceOf[Type[_ <: Tuple]])
        val f = '{ ((u: U) => $rpm.fromProduct(u.asInstanceOf[Product])).asInstanceOf[Any => Any] }
        val g = '{ ((r: R) => $upm.fromProduct(r.asInstanceOf[Product])).asInstanceOf[Any => Any] }
        (f, g, elemTpes)
      case _ =>
        utpe match {
          case utpe @ '[ HList ] =>
            val elemTpes = decomposeHList(utpe)
            val f = '{ ((u: U) => $rpm.fromProduct(u.asInstanceOf[Product])).asInstanceOf[Any => Any] }
            val g = '{ ((r: R) => r.asInstanceOf[Product].productIterator.foldRight(HNil: HList) { case (n, z) => new HCons(n, z) }).asInstanceOf[Any => Any] }
            (f, g, elemTpes)
          case _ if targetElemTpes.length == 1 =>
            val f = '{ ((u: U) => $rpm.fromProduct(new ArrayProduct(Array(u.asInstanceOf[AnyRef])))).asInstanceOf[Any => Any] }
            val g = '{ ((r: R) => r.asInstanceOf[Product].productElement(0)).asInstanceOf[Any => Any] }
            (f, g, targetElemTpes)
          case _ =>
            report.throwError(s"Source type ${utpe.show} must be a product, HList or single value")
        }
    }

    if(elemTpes.length != targetElemTpes.length) {
      val src = elemTpes.iterator.map(_.show).mkString("(", ", ", ")")
      val target = targetElemTpes.iterator.map(_.show).mkString("(", ", ", ")")
      report.throwError(s"Source and target product decomposition do not match.\n  Source: $src\n  Target: $target")
    }

    '{ new MappedProjection[R, U]($sv.toNode, MappedScalaType.Mapper($g, $f, None), $rct) }
  }

  // Turn ConstColumn into Rep at the top level on Dotty to avoid ClassCastExceptions
  type Unconst[P, P2] = P2 match {
    case ConstColumn[t] => Rep[t]
    case _ => P2
  }
}

trait ToTuple[E, T] extends (E => Option[T])

object ToTuple extends ToTupleLowPriority {
  implicit def someToTuple[T]: ToTuple[Some[T], T] = identity
}

trait ToTupleLowPriority {
  implicit def optionToTuple[T]: ToTuple[Option[T], T] = identity
  implicit def productToTuple[T <: Product](using m: Mirror.ProductOf[T]): ToTuple[T, m.MirroredElemTypes] =
    (x => Some(Tuple.fromProductTyped(x)))
}
