package scala.slick.jdbc

import scala.slick.compiler.{InsertCompiler, CompilerState, CodeGen}
import scala.slick.ast._
import scala.slick.driver.JdbcDriver
import scala.slick.util.SQLBuilder
import scala.slick.SlickException
import scala.slick.profile.RelationalMappingCompilerComponent

/** JDBC driver component which contains the mapping compiler and insert compiler */
trait JdbcMappingCompilerComponent extends RelationalMappingCompilerComponent { driver: JdbcDriver =>
  type RowReader = PositionedResult
  type RowWriter = PositionedParameters
  type RowUpdater = PositionedResult

  trait MappingCompiler extends super.MappingCompiler {
    def createColumnConverter(n: Node, path: Node, optionApply: Boolean, column: Option[FieldSymbol]): ResultConverter = {
      val JdbcType(ti, option) = n.nodeType.structural
      val autoInc = column.fold(false)(_.options.contains(ColumnOption.AutoInc))
      if(option) new OptionResultConverter(ti, autoInc)
      else new BaseResultConverter(ti, autoInc, column.fold(n.toString)(_.name))
    }

    class BaseResultConverter(ti: JdbcType[Any], autoInc: Boolean, name: String) extends ResultConverter {
      def read(pr: RowReader) =
        ti.nextValueOrElse(throw new SlickException("Read NULL value for ResultSet column "+name), pr)
      def update(value: Any, pr: RowUpdater) = ti.updateValue(value, pr)
      def set(value: Any, pp: RowWriter, forced: Boolean) =
        if(forced || !autoInc) ti.setValue(value, pp)
    }

    class OptionResultConverter(ti: JdbcType[Any], autoInc: Boolean) extends ResultConverter {
      def read(pr: RowReader) = ti.nextOption(pr)
      def update(value: Any, pr: RowUpdater) = ti.updateOption(value.asInstanceOf[Option[Any]], pr)
      def set(value: Any, pp: RowWriter, forced: Boolean) =
        if(forced || !autoInc) ti.setOption(value.asInstanceOf[Option[Any]], pp)
    }
  }

  /** Code generator phase for JdbcProfile-based drivers. */
  class JdbcCodeGen(f: QueryBuilder => SQLBuilder.Result) extends CodeGen with MappingCompiler {

    def apply(state: CompilerState): CompilerState = state.map(n => apply(n, state))

    def apply(node: Node, state: CompilerState): Node =
      ClientSideOp.mapResultSetMapping(node, keepType = true) { rsm =>
        val sbr = f(driver.createQueryBuilder(rsm.from, state))
        val nfrom = CompiledStatement(sbr.sql, sbr, rsm.from.nodeType)
        val nmap = CompiledMapping(compileMapping(rsm.map), rsm.map.nodeType)
        rsm.copy(from = nfrom, map = nmap).nodeTyped(rsm.nodeType)
      }
  }

  class JdbcInsertCompiler extends InsertCompiler with MappingCompiler {
    def createMapping(ins: Insert) = CompiledMapping(compileMapping(ins.map), ins.map.nodeType)
  }
}
