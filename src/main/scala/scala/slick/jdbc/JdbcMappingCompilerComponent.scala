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
    def createColumnConverter(n: Node, path: Node, option: Boolean): ResultConverter = {
      val ti = driver.typeInfoFor(n.nodeType)
      new ResultConverter {
        def read(pr: RowReader) =
          if(option) ti.nextValue(pr)
          else ti.nextValueOrElse(
            if(ti.nullable) None
            else throw new SlickException("Read NULL value for ResultSet column "+n),
            pr
          )
        def update(value: Any, pr: RowUpdater) = ti.updateValue(value, pr)
        def set(value: Any, pp: RowWriter) = ti.setValue(value, pp)
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
        val nmap = CompiledMapping(compileMapping(rsm.map), rsm.map.nodeType)
        rsm.copy(from = nfrom, map = nmap).nodeTyped(rsm.nodeType)
      }
  }

  class JdbcInsertCompiler extends InsertCompiler with MappingCompiler {
    def createMapping(ins: Insert) = CompiledMapping(compileMapping(ins.map), ins.map.nodeType)
  }
}
