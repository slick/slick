package scala.slick.relational

import scala.language.existentials
import scala.slick.ast._
import scala.slick.util.TupleSupport
import java.io.{StringWriter, OutputStreamWriter, PrintWriter}

/** A `ResultConverter` is used to read data from a result, update a result,
  * and set parameters of a query. */
abstract class ResultConverter[M <: ResultConverterDomain, T] {
  protected[this] type Reader = M#Reader
  protected[this] type Writer = M#Writer
  protected[this] type Updater = M#Updater
  def readGeneric(pr: Reader): T
  def updateGeneric(value: T, pr: Updater): Unit
  def setGeneric(value: T, pp: Writer, forced: Boolean): Unit
  def info: String = {
    val cln = getClass.getName.replaceAll(".*\\.", "")
    val sep = cln.lastIndexOf("_")
    if(sep == -1) Dump.green + cln + Dump.normal
    else Dump.green + cln.substring(0, sep) + Dump.normal + "_" + Dump.yellow + cln.substring(sep+1) + Dump.normal
  }
  def children: Iterator[ResultConverter[M, _]] = Iterator.empty
  override def toString = s"$info(${children.mkString(", ")}})"
}

object ResultConverter {
  /** Write a dump of a `ResultConverter` hierarchy to a provided `PrintWriter` or `System.out` */
  def dump(r: ResultConverter[_ <: ResultConverterDomain, _], prefix: String = "", to: PrintWriter = null): Unit = {
    val out = if(to eq null) new PrintWriter(new OutputStreamWriter(System.out)) else to
    dumpInternal(r, prefix, out)
    out.flush()
  }

  def getDump(r: ResultConverter[_ <: ResultConverterDomain, _], prefix: String = ""): String = {
    val buf = new StringWriter
    dump(r, prefix, new PrintWriter(buf))
    buf.getBuffer.toString
  }

  private[this] def dumpInternal(r: ResultConverter[_ <: ResultConverterDomain, _], prefix: String, to: PrintWriter): Unit = {
    to.println(prefix + r.info)
    r.children.foreach { ch => dumpInternal(ch, prefix + "  ", to) }
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
  override def children = elementConverters.iterator
  private[this] val cha = children.to[Array]
  private[this] val len = cha.length
  def readGeneric(pr: Reader) = {
    val a = new Array[Any](len)
    var i = 0
    while(i < len) {
      a(i) = cha(i).readGeneric(pr)
      i += 1
    }
    TupleSupport.buildTuple(a).asInstanceOf[T]
  }
  def updateGeneric(value: T, pr: Updater) = {
    var i = 0
    while(i < len) {
      cha(i).asInstanceOf[ResultConverter[M, Any]].updateGeneric(value.productElement(i), pr)
      i += 1
    }
  }
  def setGeneric(value: T, pp: Writer, forced: Boolean) = {
    var i = 0
    while(i < len) {
      cha(i).asInstanceOf[ResultConverter[M, Any]].setGeneric(value.productElement(i), pp, forced)
      i += 1
    }
  }
}

final class GetOrElseResultConverter[M <: ResultConverterDomain, T](child: ResultConverter[M, Option[T]], default: () => T) extends ResultConverter[M, T] {
  def readGeneric(pr: Reader) = child.readGeneric(pr).getOrElse(default())
  def updateGeneric(value: T, pr: Updater) = child.updateGeneric(Some(value), pr)
  def setGeneric(value: T, pp: Writer, forced: Boolean) = child.setGeneric(Some(value), pp, forced)
  override def info =
    super.info + s"(${ try default() catch { case e: Throwable => "["+e.getClass.getName+"]" } })"
  override def children = Iterator(child)
}

final case class TypeMappingResultConverter[M <: ResultConverterDomain, T, C](child: ResultConverter[M, C], toBase: T => C, toMapped: C => T) extends ResultConverter[M, T] {
  def readGeneric(pr: Reader) = toMapped(child.readGeneric(pr))
  def updateGeneric(value: T, pr: Updater) = child.updateGeneric(toBase(value), pr)
  def setGeneric(value: T, pp: Writer, forced: Boolean) = child.setGeneric(toBase(value), pp, forced)
  override def children = Iterator(child)
}
