
package scala.slick.mongodb

import scala.slick.profile.{RelationalProfile, Capability, RelationalMappingCompilerComponent, RelationalDriver}
import scala.slick.ast._
import scala.slick.SlickException
import scala.slick.compiler.{InsertCompiler, CompilerState, CodeGen}
import scala.slick.ast.ElementSymbol
import scala.slick.ast.StructType
import scala.slick.ast.ProductType
import scala.slick.ast.CollectionType
import scala.slick.ast.Select
import scala.slick.ast.Insert
import scala.collection.mutable.ArrayBuffer
import scala.slick.util.SlickLogger
import org.slf4j.LoggerFactory

/**
 * Slick driver for MongoDB
 *
 * Based on Casbah, Fully synchronous. A rough sketch of the ultimate plan for a full fledged
 * MongoDB mapping
 *
 * @author bwmcadams
 */
class MongoDriver extends RelationalDriver
                  with MongoProfile
                  with MongoTypesComponent
                  with RelationalMappingCompilerComponent { driver =>
  override protected[this] lazy val logger = new SlickLogger(LoggerFactory.getLogger(classOf[MongoDriver]))

  type RowReader = ArrayBuffer[Any]  // MongoResult
  type RowWriter = ArrayBuffer[Any] // MongoInserter
  type RowUpdater = ArrayBuffer[Any] // MongoUpdater

  override val profile: MongoProfile = this


  // TODO - Detect Mongo JS Engine and access to aggregation at connection time

  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - RelationalProfile.capabilities.foreignKeyActions /** we might be able to gerryMander this around DBRefs, later */
    - RelationalProfile.capabilities.functionDatabase /** I think we need to be able to do this in an exact SQL Way...? */
    - RelationalProfile.capabilities.functionUser
    - RelationalProfile.capabilities.joinFull
    - RelationalProfile.capabilities.joinRight
    - RelationalProfile.capabilities.likeEscape
    - RelationalProfile.capabilities.pagingDrop
    - RelationalProfile.capabilities.pagingNested
    - RelationalProfile.capabilities.pagingPreciseTake
    - RelationalProfile.capabilities.typeBigDecimal /** MongoDB has no safe type mapping for BigDecimal */
    - RelationalProfile.capabilities.zip /** TODO - talk to Stefan about what zip capabilities means/need */
  )

  trait QueryMappingCompiler extends super.MappingCompiler {
    def createColumnConverter(n: Node, path: Node, option: Boolean): ResultConverter = { // todo - fill me in
      val Select(_, ElementSymbol(ridx)) = path
      val nullable = typeInfoFor(n.nodeType).nullable
      new ResultConverter {
        def read(pr: RowReader) = {
          val v = pr(ridx-1)
          if (!nullable && (v.asInstanceOf[AnyRef] eq null)) throw new SlickException("Read null value for non-nullable column")
          v
        }
        def update(value: Any, pr: RowUpdater) = ???
        def set(value: Any, pp: RowWriter) = ???
      }
    }
  }

  class MongoCodeGen extends CodeGen with QueryMappingCompiler {
    def apply(state: CompilerState): CompilerState = state.map(n => retype(apply(n, state)))

    def apply(node: Node, state: CompilerState): Node = // todo - evaluate. Should still be CSO?
      ClientSideOp.mapResultSetMapping(node, keepType = true) { rsm =>
        val nmap = CompiledMapping(compileMapping(rsm.map), rsm.map.nodeType)
        rsm.copy(map = nmap).nodeTyped(rsm.nodeType)
      }

    def retype(n: Node): Node = {
      val n2 = n.nodeMapChildrenKeepType(retype)
      n2.nodeRebuildWithType(trType(n2.nodeType))
    }

    def trType(t: Type): Type = t match {
      case StructType(el) => StructType(el.map { case (s, t) => (s, trType(t)) })
      case ProductType(el) => ProductType(el.map(trType))
      case CollectionType(cons, el) => CollectionType(cons, trType(el))
      case t => typeInfoFor(t)
    }


  }


  class InsertMappingCompiler(insert: Insert) extends super.MappingCompiler {
    val Insert(_, table: Table[_], _, ProductNode(cols)) = insert
    val tableColumnIdxs = table.create_*.zipWithIndex.toMap // TODO - Choosing queries based on indexes is going to be massively important

    def createColumnConverter(n: Node, path: Node, option: Boolean): ResultConverter = {
      logger.debug(s"Creating Column Converter for Node: '$n', Path: '$path', Option: '$option'")
      val Select(_, ElementSymbol(ridx)) = path
      val Select(_, fs: FieldSymbol) = cols(ridx - 1)
      val tidx = tableColumnIdxs(fs)
      new ResultConverter {
        def read(pr: RowReader) = ???
        def update(value: Any, pr: RowUpdater) = ???
        def set(value: Any, pp: RowWriter) = pp(tidx) = value
      }
    }
  }

  class MongoInsertCompiler extends InsertCompiler {
    def createMapping(ins: Insert) = CompiledMapping(new InsertMappingCompiler(ins).compileMapping(ins.map), ins.map.nodeType)
  }
}

object MongoDriver extends MongoDriver
