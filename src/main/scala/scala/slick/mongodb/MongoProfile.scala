package scala.slick.mongodb

import com.mongodb.casbah.Imports._
import scala.slick.profile.{RelationalProfile, Capability}



trait MongoProfile extends RelationalProfile { driver: MongoDriver =>
  type Backend = MongoBackend

  type ColumnType[T] =
}

object MongoProfile {
  object capabilities {
    /** Supports aggregation framework */
    val aggregation = Capability("mongodb.aggregation")
    /** Supports the V8 JavaScript engine */
    val v8JSEngine = Capability("mongodb.jsengine.v8")
    /** Supports the SpiderMonkey JavaScript engine  (v8 is better and allows more features ) */
    val spiderMonkeyJSEngine = Capability("mongodb.jsengine.spidermonkey")

    /** Supports all MongoDB features which do not have separate capability values */
    val other = Capability("mongodb.other")

    /** All MongoDB capabilities */
    val all = Set(aggregation, v8JSEngine, spiderMonkeyJSEngine, other)
  }
}

/**
 * Slick driver for MongoDB
 *
 * Based on Casbah, Fully synchronous. A rough sketch of the ultimate plan for a full fledged
 * MongoDB mapping
 *
 * @author bwmcadams
 */
class MongoDriver extends RelationalDriver with MongoProfile { driver =>

  type SchemaDescriptionDef

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


}
