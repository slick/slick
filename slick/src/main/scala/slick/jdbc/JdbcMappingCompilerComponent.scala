package slick.jdbc

import java.sql.{PreparedStatement, ResultSet}

import slick.ast.*
import slick.compiler.{CodeGen, CompilerState}
import slick.relational.*
import slick.util.SQLBuilder

/** JDBC profile component which contains the mapping compiler and insert compiler */
trait JdbcMappingCompilerComponent { self: JdbcProfile =>

  /** The `MappingCompiler` for this profile. */
  val mappingCompiler: MappingCompiler = new MappingCompiler

  /** Create a (possibly specialized) `ResultConverter` for the given `JdbcType`. */
  def createBaseResultConverter[T](ti: JdbcType[T],
                                   name: String,
                                   idx: Int): ResultConverter[JdbcResultConverterDomain, T] =
    SpecializedJdbcResultConverter.base(ti, name, idx)

  /** Create a (possibly specialized) `ResultConverter` for `Option` values of the given `JdbcType`. */
  def createOptionResultConverter[T](ti: JdbcType[T], idx: Int): ResultConverter[JdbcResultConverterDomain, Option[T]] =
    new OptionResultConverter(ti, idx)

  /** A ResultConverterCompiler that builds JDBC-based converters. Instances of
    * this class use mutable state internally. They are meant to be used for a
    * single conversion only and must not be shared or reused. */
  class MappingCompiler extends ResultConverterCompiler[JdbcResultConverterDomain] {
    def createColumnConverter(n: Node,
                              idx: Int,
                              column: Option[FieldSymbol]): ResultConverter[JdbcResultConverterDomain, ?] = {
      val JdbcType(ti, option) = n.nodeType.structural
      if(option) createOptionResultConverter(ti, idx)
      else createBaseResultConverter(ti, column.fold("<computed>")(_.name), idx)
    }

    override def createGetOrElseResultConverter[T](rc: ResultConverter[JdbcResultConverterDomain, Option[T]],
                                                   default: () => T) = rc match {
      case rc: OptionResultConverter[?] => rc.getOrElse(default)
      case _                            => super.createGetOrElseResultConverter[T](rc, default)
    }

    override def createIsDefinedResultConverter[T](rc: ResultConverter[JdbcResultConverterDomain, Option[T]]) =
      rc match {
        case rc: OptionResultConverter[?] => rc.isDefined
        case _                            => super.createIsDefinedResultConverter(rc)
      }

    override def createTypeMappingResultConverter(rc: ResultConverter[JdbcResultConverterDomain, Any],
                                                  mapper: MappedScalaType.Mapper) = {
      val tm = TypeMappingResultConverter(rc, mapper.toBase, mapper.toMapped)
      mapper.fastPath match {
        case Some(f) => f(tm).asInstanceOf[ResultConverter[JdbcResultConverterDomain, Any]]
        case None => tm
      }
    }
  }

  /** Code generator phase for queries on JdbcProfile. */
  class JdbcCodeGen(f: QueryBuilder => SQLBuilder.Result) extends CodeGen {
    override def compileServerSideAndMapping(serverSide: Node,
                                             mapping: Option[Node],
                                             state: CompilerState): (CompiledStatement, Option[CompiledMapping]) = {
      val (tree, tpe) = treeAndType(serverSide)
      val sbr = f(self.createQueryBuilder(tree, state))
      (CompiledStatement(sbr.sql, sbr, tpe).infer(), mapping.map(mappingCompiler.compileMapping))
    }
  }

  /** Code generator phase for inserts on JdbcProfile. */
  class JdbcInsertCodeGen(f: Insert => InsertBuilder) extends CodeGen {
    override def compileServerSideAndMapping(serverSide: Node,
                                             mapping: Option[Node],
                                             state: CompilerState): (CompiledStatement, Option[CompiledMapping]) = {
      val ib = f(serverSide.asInstanceOf[Insert])
      val ibr = ib.buildInsert
      (CompiledStatement(ibr.sql, ibr, serverSide.nodeType).infer(),
        mapping.map(n => mappingCompiler.compileMapping(ib.transformMapping(n))))
    }
  }
}

trait JdbcResultConverterDomain extends ResultConverterDomain {
  type Reader = ResultSet
  type Writer = PreparedStatement
  type Updater = ResultSet
}
