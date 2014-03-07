package scala.slick.jdbc

import scala.slick.compiler.{InsertCompiler, CompilerState, CodeGen}
import scala.slick.ast._
import scala.slick.relational._
import scala.slick.lifted.MappedProjection
import scala.slick.driver.JdbcDriver
import scala.slick.util.SQLBuilder

/** JDBC driver component which contains the mapping compiler and insert compiler */
trait JdbcMappingCompilerComponent { driver: JdbcDriver =>

  trait MappingCompiler extends ResultConverterCompiler[JdbcResultConverterDomain] {
    def createColumnConverter(n: Node, path: Node, optionApply: Boolean, column: Option[FieldSymbol]): ResultConverter[JdbcResultConverterDomain, _] = {
      val JdbcType(ti, option) = n.nodeType.structural
      val autoInc = column.fold(false)(_.options.contains(ColumnOption.AutoInc))
      if(option) OptionResultConverter(ti, autoInc)
      else BaseResultConverter(ti, autoInc, column.fold(n.toString)(_.name))
    }

    override def createGetOrElseResultConverter[T](rc: ResultConverter[JdbcResultConverterDomain, Option[T]], default: () => T) =
      DefaultingResultConverter[T](rc, default)

    override def createTypeMappingResultConverter(rc: ResultConverter[JdbcResultConverterDomain, Any], mapper: MappedScalaType.Mapper) = {
      val tm = new TypeMappingResultConverter(rc, mapper.toBase, mapper.toMapped)
      mapper.fastPath match {
        case Some(pf) => pf.orElse[Any, Any] { case x => x }.apply(tm).asInstanceOf[ResultConverter[JdbcResultConverterDomain, Any]]
        case None => tm
      }
    }
  }

  /** Code generator phase for JdbcProfile-based drivers. */
  class JdbcCodeGen(f: QueryBuilder => SQLBuilder.Result) extends CodeGen with MappingCompiler {

    def apply(state: CompilerState): CompilerState = state.map(n => apply(n, state))

    def apply(node: Node, state: CompilerState): Node =
      ClientSideOp.mapResultSetMapping(node, keepType = true) { rsm =>
        val sbr = f(driver.createQueryBuilder(rsm.from, state))
        val nfrom = CompiledStatement(sbr.sql, sbr, rsm.from.nodeType)
        val nmap = compileMapping(rsm.map)
        rsm.copy(from = nfrom, map = nmap).nodeTyped(rsm.nodeType)
      }
  }

  class JdbcInsertCompiler extends InsertCompiler with MappingCompiler {
    def createMapping(ins: Insert) = compileMapping(ins.map)
  }

  class JdbcFastPathExtensionMethods[T, P](val mp: MappedProjection[T, P]) {
    def fastPath(fpf: (TypeMappingResultConverter[JdbcResultConverterDomain, T, _] => JdbcFastPath[T])): MappedProjection[T, P] = mp.genericFastPath {
      case tm @ TypeMappingResultConverter(_: ProductResultConverter[_, _], _, _) =>
        fpf(tm.asInstanceOf[TypeMappingResultConverter[JdbcResultConverterDomain, T, _]])

    }
  }
}

trait JdbcResultConverterDomain extends ResultConverterDomain {
  type Reader = PositionedResult
  type Writer = PositionedParameters
  type Updater = PositionedResult
}
