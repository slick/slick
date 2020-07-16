package slick.relational

import slick.SlickException
import slick.util.{Dumpable, DumpInfo, TupleSupport}

/** A `ResultConverter` is used to read data from a result, update a result,
  * and set parameters of a query. */
trait ResultConverter[M <: ResultConverterDomain, T] extends Dumpable {
  protected[this] type Reader = M#Reader
  protected[this] type Writer = M#Writer
  protected[this] type Updater = M#Updater
  def read(pr: Reader): T
  def update(value: T, pr: Updater): Unit

  /**
   * Writes converted value.
   *
   * @param offset The number of `PreparedStatement` parameters to skip.
   *               In a single-row operation (ex. insert into tbl values (?, ?, ?)), it should be 0.
   *               In a bulk insert operation, it should be ''C'' × ''R'', where ''C'' is the number of columns
   *               and ''R'' is the number of rows that have already been set.
   */
  def set(value: T, pp: Writer, offset: Int): Unit

  override def toString = {
    val di = getDumpInfo
    di.name + "(" + di.children.map(_._2).mkString(", ") + ")"
  }

  /** The width of this converter (in columns), corresponding to the
    * number of columns that will be read or written by it. */
  def width: Int

  override def getDumpInfo = {
    val cln = DumpInfo.simpleNameFor(getClass)
    val sep = cln.indexOf("$mc")
    val name = if(sep == -1) cln else cln.substring(0, sep) + DumpInfo.highlight(cln.substring(sep))
    DumpInfo(name)
  }
}

/** The domain of a `ResultConverter` and associated classes. It defines the
  * `Reader`, `Writer` and `Updater` types that are needed at the lowest
  * level of ResultConverters for accessing the underlying profile-specific
  * data structures. */
trait ResultConverterDomain {
  type Reader
  type Writer
  type Updater
}

/** An efficient (albeit boxed) ResultConverter for Product/Tuple values. */
final case class ProductResultConverter[
  M <: ResultConverterDomain,
  T <: Product
](elementConverters: ResultConverter[M, ?]*) extends ResultConverter[M, T] {
  private[this] val cha = elementConverters.toArray
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

  def set(value: T, pp: Writer, offset: Int) = {
    var i = 0
    while(i < len) {
      cha(i).asInstanceOf[ResultConverter[M, Any]].set(value.productElement(i), pp, offset)
      i += 1
    }
  }

  override def getDumpInfo =
    super.getDumpInfo.copy(children = elementConverters.zipWithIndex.map { case (ch, i) => ((i+1).toString, ch) })
}

/** Result converter that can write to multiple sub-converters and read from the first one */
final case class CompoundResultConverter[M <: ResultConverterDomain, T
](width: Int, childConverters: ResultConverter[M, T]*) extends ResultConverter[M, T] {
  private[this] val cha = childConverters.toArray
  private[this] val len = cha.length

  def read(pr: Reader) = {
    if (len == 0) throw new SlickException("Cannot read from empty CompoundResultConverter")
    else cha(0).read(pr)
  }
  def update(value: T, pr: Updater) = {
    var i = 0
    while (i < len) {
      cha(i).update(value, pr)
      i += 1
    }
  }
  def set(value: T, pp: Writer, offset: Int) = {
    var i = 0
    while (i < len) {
      cha(i).set(value, pp, offset)
      i += 1
    }
  }

  override def getDumpInfo = super.getDumpInfo.copy(children = childConverters.zipWithIndex.map {
    case (ch, i) => (if (i == 0) "*" else "-", ch)
  })
}

final class UnitResultConverter[M <: ResultConverterDomain] extends ResultConverter[M, Unit] {
  def width = 0
  def read(pr: Reader) = ()
  def update(value: Unit, pr: Updater) = ()
  def set(value: Unit, pp: Writer, offset: Int) = ()
}

final class GetOrElseResultConverter[M <: ResultConverterDomain, T](child: ResultConverter[M, Option[T]],
                                                                    default: () => T) extends ResultConverter[M, T] {
  def read(pr: Reader) = child.read(pr).getOrElse(default())
  def update(value: T, pr: Updater) = child.update(Some(value), pr)
  def set(value: T, pp: Writer, offset: Int) = child.set(Some(value), pp, offset)
  def width = child.width
  override def getDumpInfo =
    super.getDumpInfo.copy(
      mainInfo =
        try default().toString
        catch {
          case e: Throwable => "[" + e.getClass.getName + "]"
        },
      children = Vector(("child", child))
    )
}

final class IsDefinedResultConverter[M <: ResultConverterDomain](child: ResultConverter[M, Option[?]])
  extends ResultConverter[M, Boolean] {

  def read(pr: Reader) = child.read(pr).isDefined
  override def update(value: Boolean, pr: Updater): Nothing =
    throw new SlickException("Cannot insert/update IsDefined check")
  override def set(value: Boolean, pp: Writer, offset: Int): Nothing =
    throw new SlickException("Cannot insert/update IsDefined check")
  def width = child.width
  override def getDumpInfo =
    super.getDumpInfo.copy(children = Vector(("child", child)))
}

final case class TypeMappingResultConverter[M <: ResultConverterDomain, T, C](child: ResultConverter[M, C],
                                                                              toBase: T => C,
                                                                              toMapped: C => T)
  extends ResultConverter[M, T] {
  def read(pr: Reader) = toMapped(child.read(pr))
  def update(value: T, pr: Updater) = child.update(toBase(value), pr)
  def set(value: T, pp: Writer, offset: Int) = child.set(toBase(value), pp, offset)
  def width = child.width
  override def getDumpInfo = super.getDumpInfo.copy(children = Vector(("child", child)))
}

final case class OptionRebuildingResultConverter[
  M <: ResultConverterDomain,
  T
](discriminator: ResultConverter[M, Boolean], data: ResultConverter[M, T]) extends ResultConverter[M, Option[T]] {
  def read(pr: Reader): Option[T] =
    if (discriminator.read(pr)) Some(data.read(pr)) else None
  def update(value: Option[T], pr: Updater): Nothing =
    throw new SlickException("Cannot insert/update non-primitive Option value")
  def set(value: Option[T], pp: Writer, offset: Int): Nothing =
    throw new SlickException("Cannot insert/update non-primitive Option value")
  def width = discriminator.width + data.width
  override def getDumpInfo = super.getDumpInfo.copy(children = Vector(("discriminator", discriminator), ("data", data)))
}

/** A `ResultConverter` that simplifies the implementation of fast path
 * converters. It always wraps a `TypeMappingResultConverter`
 * on top of a `ProductResultConverter`, allowing direct access to the product
 * elements. */
abstract class SimpleFastPathResultConverter[
  M <: ResultConverterDomain,
  T
](protected[this] val rc: TypeMappingResultConverter[M, T, ?]) extends ResultConverter[M, T] {
  private[this] val ch = rc.child.asInstanceOf[ProductResultConverter[M, ?]].elementConverters
  private[this] var idx = -1

  /** Return the next specialized child `ResultConverter` for the specified type. */
  protected[this] def next[C] = {
    idx += 1
    ch(idx).asInstanceOf[ResultConverter[M, C]]
  }

  def read(pr: Reader) = rc.read(pr)
  def update(value: T, pr: Updater) = rc.update(value, pr)
  def set(value: T, pp: Writer, offset: Int) = rc.set(value, pp, offset)

  override def getDumpInfo =
    super.getDumpInfo.copy(name = "SimpleFastPathResultConverter", mainInfo = "", children = Vector(("rc", rc)))
  def width = rc.width
}
