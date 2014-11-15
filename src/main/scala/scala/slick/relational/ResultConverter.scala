package scala.slick.relational

import scala.language.existentials
import scala.slick.SlickException
import scala.slick.ast._
import scala.slick.util.{Dumpable, DumpInfo, TupleSupport}
import java.io.{StringWriter, OutputStreamWriter, PrintWriter}

/** A `ResultConverter` is used to read data from a result, update a result,
  * and set parameters of a query. */
trait ResultConverter[M <: ResultConverterDomain, @specialized T] extends Dumpable {
  protected[this] type Reader = M#Reader
  protected[this] type Writer = M#Writer
  protected[this] type Updater = M#Updater
  def read(pr: Reader): T
  def update(value: T, pr: Updater): Unit
  def set(value: T, pp: Writer): Unit
  override def toString = {
    val di = getDumpInfo
    di.name + "(" + di.children.map(_._2).mkString(", ") + ")"
  }

  /** The width of this converter (in columns), corresponding to the
    * number of columns that will be read or written by it. */
  def width: Int

  override def getDumpInfo = {
    val cln = getClass.getName.replaceAll(".*\\.", "")
    val sep = cln.indexOf("$mc")
    val name = if(sep == -1) cln else cln.substring(0, sep) + DumpInfo.highlight(cln.substring(sep))
    DumpInfo(name)
  }
}

/** The domain of a `ResultConverter` and associated classes. It defines the
  * `Reader`, `Writer` and `Updater` types that are needed at the lowest
  * level of ResultConverters for accessing the underlying driver-specific
  * data structures. */
trait ResultConverterDomain {
  type Reader
  type Writer
  type Updater
}

/** An efficient (albeit boxed) ResultConverter for Product/Tuple values. */
final case class ProductResultConverter[M <: ResultConverterDomain, T <: Product](elementConverters: ResultConverter[M, _]*) extends ResultConverter[M, T] {
  private[this] val cha = elementConverters.to[Array]
  private[this] val len = cha.length

  val width = cha.foldLeft(0)(_ + _.width)

  def read(pr: Reader) = {
    val a = new Array[Any](len)
    var i = 0
    while(i < len) {
      a(i) = cha(i).read(pr)
      i += 1
    }
    TupleSupport.buildTuple(a).asInstanceOf[T]
  }
  def update(value: T, pr: Updater) = {
    var i = 0
    while(i < len) {
      cha(i).asInstanceOf[ResultConverter[M, Any]].update(value.productElement(i), pr)
      i += 1
    }
  }
  def set(value: T, pp: Writer) = {
    var i = 0
    while(i < len) {
      cha(i).asInstanceOf[ResultConverter[M, Any]].set(value.productElement(i), pp)
      i += 1
    }
  }

  override def getDumpInfo = super.getDumpInfo.copy(children = elementConverters.zipWithIndex.map { case (ch, i) => ((i+1).toString, ch) })
}

/** Result converter that can write to multiple sub-converters and read from the first one */
final case class CompoundResultConverter[M <: ResultConverterDomain, @specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean) T](width: Int, childConverters: ResultConverter[M, T]*) extends ResultConverter[M, T] {
  private[this] val cha = childConverters.to[Array]
  private[this] val len = cha.length

  def read(pr: Reader) = {
    if(len == 0) throw new SlickException("Cannot read from empty CompoundResultConverter")
    else cha(0).read(pr)
  }
  def update(value: T, pr: Updater) = {
    var i = 0
    while(i < len) {
      cha(i).update(value, pr)
      i += 1
    }
  }
  def set(value: T, pp: Writer) = {
    var i = 0
    while(i < len) {
      cha(i).set(value, pp)
      i += 1
    }
  }

  override def getDumpInfo = super.getDumpInfo.copy(children = childConverters.zipWithIndex.map {
    case (ch, i) => (if(i == 0) "*" else "-", ch)
  })
}

final class UnitResultConverter[M <: ResultConverterDomain] extends ResultConverter[M, Unit] {
  def width = 0
  def read(pr: Reader) = ()
  def update(value: Unit, pr: Updater) = ()
  def set(value: Unit, pp: Writer) = ()
}

final class GetOrElseResultConverter[M <: ResultConverterDomain, T](child: ResultConverter[M, Option[T]], default: () => T) extends ResultConverter[M, T] {
  def read(pr: Reader) = child.read(pr).getOrElse(default())
  def update(value: T, pr: Updater) = child.update(Some(value), pr)
  def set(value: T, pp: Writer) = child.set(Some(value), pp)
  def width = child.width
  override def getDumpInfo =
    super.getDumpInfo.copy(mainInfo = (try default().toString catch { case e: Throwable => "["+e.getClass.getName+"]" }), children = Vector(("child", child)))
}

final case class TypeMappingResultConverter[M <: ResultConverterDomain, T, C](child: ResultConverter[M, C], toBase: T => C, toMapped: C => T) extends ResultConverter[M, T] {
  def read(pr: Reader) = toMapped(child.read(pr))
  def update(value: T, pr: Updater) = child.update(toBase(value), pr)
  def set(value: T, pp: Writer) = child.set(toBase(value), pp)
  def width = child.width
  override def getDumpInfo = super.getDumpInfo.copy(children = Vector(("child", child)))
}

final case class OptionRebuildingResultConverter[M <: ResultConverterDomain, T](discriminator: ResultConverter[M, Int], data: ResultConverter[M, T]) extends ResultConverter[M, Option[T]] {
  def read(pr: Reader): Option[T] = discriminator.read(pr) match {
    case 1 => Some(data.read(pr))
    case _ => None
  }
  def update(value: Option[T], pr: Updater) =
    value.fold(throw new SlickException("Cannot insert/update non-primitive Option value None")) { v =>
      discriminator.update(1, pr)
      data.update(v, pr)
    }
  def set(value: Option[T], pp: Writer) =
    value.fold(throw new SlickException("Cannot insert/update non-primitive Option value None")) { v =>
      discriminator.set(1, pp)
      data.set(v, pp)
    }
  def width = discriminator.width + data.width
  override def getDumpInfo = super.getDumpInfo.copy(children = Vector(("discriminator", discriminator), ("data", data)))
}
