package scala.slick.jdbc

import java.sql.{PreparedStatement, ResultSet}
import scala.slick.compiler.{CompilerState, CodeGen}
import scala.slick.ast._
import scala.slick.relational._
import scala.slick.lifted.MappedProjection
import scala.slick.driver.JdbcDriver
import scala.slick.util.SQLBuilder

/** JDBC driver component which contains the mapping compiler and insert compiler */
trait JdbcMappingCompilerComponent { driver: JdbcDriver =>

  /** Create a `MappingCompiler` for this driver. */
  def createMappingCompiler: MappingCompiler = new MappingCompiler

  /** Create a (possibly specialized) `ResultConverter` for the given `JdbcType`. */
  def createBaseResultConverter[T](ti: JdbcType[T], name: String, idx: Int): ResultConverter[JdbcResultConverterDomain, T] =
    SpecializedJdbcResultConverter.base(ti, name, idx)

  /** Create a (possibly specialized) `ResultConverter` for `Option` values of the given `JdbcType`. */
  def createOptionResultConverter[T](ti: JdbcType[T], idx: Int): ResultConverter[JdbcResultConverterDomain, Option[T]] =
    SpecializedJdbcResultConverter.option(ti, idx)

  /** A ResultConverterCompiler that builds JDBC-based converters. Instances of
    * this class use mutable state internally. They are meant to be used for a
    * single conversion only and must not be shared or reused. */
  class MappingCompiler extends ResultConverterCompiler[JdbcResultConverterDomain] {
    protected[this] var idx = 0

    def createColumnConverter(n: Node, path: Node, column: Option[FieldSymbol]): ResultConverter[JdbcResultConverterDomain, _] = {
      val JdbcType(ti, option) = n.nodeType.structural
      idx += 1
      if(option) createOptionResultConverter(ti, idx)
      else createBaseResultConverter(ti, column.fold(n.toString)(_.name), idx)
    }

    override def createGetOrElseResultConverter[T](rc: ResultConverter[JdbcResultConverterDomain, Option[T]], default: () => T) = rc match {
      case rc: OptionResultConverter[_] => rc.getOrElse(default)
      case _ => super.createGetOrElseResultConverter[T](rc, default)
    }

    override def createTypeMappingResultConverter(rc: ResultConverter[JdbcResultConverterDomain, Any], mapper: MappedScalaType.Mapper) = {
      val tm = new TypeMappingResultConverter(rc, mapper.toBase, mapper.toMapped)
      mapper.fastPath match {
        case Some(pf) => pf.orElse[Any, Any] { case x => x }.apply(tm).asInstanceOf[ResultConverter[JdbcResultConverterDomain, Any]]
        case None => tm
      }
    }
  }

  /** Code generator phase for queries on JdbcProfile-based drivers. */
  class JdbcCodeGen(f: QueryBuilder => SQLBuilder.Result) extends CodeGen {
    def compileServerSide(n: Node, state: CompilerState) = {
      val sbr = f(driver.createQueryBuilder(n, state))
      CompiledStatement(sbr.sql, sbr, n.nodeType)
    }
    def compileMapping(n: Node, state: CompilerState, serverSide: Node) = createMappingCompiler.compileMapping(n)
  }

  /** Code generator phase for inserts on JdbcProfile-based drivers. */
  class JdbcInsertCodeGen extends CodeGen {
    override def compileServerSide(n: Node, state: CompilerState) = {
      val ibr = driver.createInsertBuilder(n.asInstanceOf[Insert]).buildInsert
      CompiledStatement(ibr.sql, ibr, n.nodeType)
    }
    def compileMapping(n: Node, state: CompilerState, serverSide: Node) = createMappingCompiler.compileMapping(n)
  }

  class JdbcFastPathExtensionMethods[T, P](val mp: MappedProjection[T, P]) {
    def fastPath(fpf: (TypeMappingResultConverter[JdbcResultConverterDomain, T, _] => JdbcFastPath[T])): MappedProjection[T, P] = mp.genericFastPath {
      case tm @ TypeMappingResultConverter(_: ProductResultConverter[_, _], _, _) =>
        fpf(tm.asInstanceOf[TypeMappingResultConverter[JdbcResultConverterDomain, T, _]])

    }
  }
}

trait JdbcResultConverterDomain extends ResultConverterDomain {
  type Reader = ResultSet
  type Writer = PreparedStatement
  type Updater = ResultSet
}
