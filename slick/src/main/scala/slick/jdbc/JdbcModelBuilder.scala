package slick.jdbc


import java.sql.DatabaseMetaData

import scala.reflect.ClassTag
import scala.util.{Failure, Success}

import slick.{SlickException, model as m}
import slick.ast.ColumnOption
import slick.dbio.*
import slick.jdbc.meta.*
import slick.relational.RelationalProfile
import slick.sql.SqlProfile
import slick.util.Logging

/** Build a Slick model from introspecting the JDBC metadata.
  *
  * In most cases you are better off transforming the generated model instead of overriding functionality here. It is
  * only useful if you need easy access to the JDBC metadata in order to influence how the model is generated. A good
  * use case would be interpreting column types or default values that Slick doesn't understand out of the box. If you
  * just want to remove or hard code some default values, transform the resulting model instead.
  *
  * The tight coupling can easily lead to source code incompatibilities in future versions. Avoid hooking in here if you
  * don't have to.
  *
  * @param ignoreInvalidDefaults see JdbcModelBuilder#ColumnBuilder#default
  */
class JdbcModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean) extends Logging {

  ////////////////////////////////////////////////////////////////////// Actions for reading the required JDBC metadata

  /** Read the column metadata for a table in ordinal position order */
  def readColumns(t: MTable): DBIO[Vector[MColumn]] = t.getColumns.map(_.sortBy(_.ordinalPosition))

  /** Read the primary key metadata for a table in key sequence order */
  def readPrimaryKeys(t: MTable): DBIO[Vector[MPrimaryKey]] = t.getPrimaryKeys.map(_.sortBy(_.keySeq))

  /** Read the foreign key metadata for a table grouped by name and in key sequence order */
  def readForeignKeys(t: MTable): DBIO[Seq[Seq[MForeignKey]]] = t.getImportedKeys.map(
    // remove foreign keys pointing to tables which were not included
    _.filter(fk => tableNamersByQName.isDefinedAt(fk.pkTable))
    .groupBy(fk => (fk.pkTable,fk.fkName,fk.pkName,fk.fkTable))
    .toSeq
    .sortBy{case (key,_) => (key._1.name,key._2,key._3,key._4.name)}
    .map(_._2.sortBy(_.keySeq)) // respect order
  )

  /** Read the index metadata grouped by name and in ordinal position order */
  def readIndices(t: MTable): DBIO[Seq[Seq[MIndexInfo]]] = t.getIndexInfo().asTry.map {
    case Success(iis) =>
      iis.groupBy(_.indexName).toSeq.sortBy(_._1).map(_._2.sortBy(_.ordinalPosition)) // respect order
    case Failure(e: java.sql.SQLException) => // TODO: this needs a test!
      logger.debug(s"Skipping indices of table ${t.name.name} due to exception during getIndexInfo: "+e.getMessage.trim)
      Seq()
    case Failure(e) => throw e
  }

  ///////////////////////////////////////////////////////////////////////////////////////////// Builder factory methods

  /** Table namer factory. Override for customization.
    * @group Basic customization overrides */
  def createTableNamer(meta: MTable): TableNamer = new TableNamer(meta)

  /** Column model builder factory. Override for customization.
    * @group Basic customization overrides */
  def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder = new ColumnBuilder(tableBuilder, meta)

  /** Primary key model builder factory. Override for customization.
    * @group Basic customization overrides */
  def createPrimaryKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MPrimaryKey]): PrimaryKeyBuilder = new PrimaryKeyBuilder(tableBuilder, meta)

  /** Foreign key model builder factory. Override for customization.
    * @group Basic customization overrides */
  def createForeignKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MForeignKey]): ForeignKeyBuilder = new ForeignKeyBuilder(tableBuilder, meta)

  /** Index model builder factory. Override for customization.
    * @group Basic customization overrides */
  def createIndexBuilder(tableBuilder: TableBuilder, meta: Seq[MIndexInfo]): IndexBuilder = new IndexBuilder(tableBuilder, meta)

  //////////////////////////////////////////////////////////////////////////////////////////////////////// Main builder

  lazy val tableNamers: Seq[TableNamer] = mTables.map(createTableNamer)
  lazy val tableNamersByQName: Map[MQName, TableNamer] = mTables.map(m => m.name).zip(tableNamers).toMap

  /** Table model builder factory. Override for customization.
    * @group Basic customization overrides */
  def createTableBuilder(namer: TableNamer): DBIO[TableBuilder] = for {
    cs <- readColumns(namer.meta)
    pks <- readPrimaryKeys(namer.meta)
    fks <- readForeignKeys(namer.meta)
    idxs <- readIndices(namer.meta)
  } yield new TableBuilder(namer.meta, namer, cs, pks, fks, idxs)

  /** Creates a Slick data model from jdbc meta data. Foreign keys pointing out of the given tables
    * are not included. */
  def buildModel: DBIO[m.Model] = for {
    ts <- DBIO.sequence(tableNamers.map(createTableBuilder))
    tablesByQName = ts.map(t => t.meta.name -> t).toMap
    builders = createBuilders(tablesByQName)
  } yield m.Model(ts.sortBy(_.meta.name.name).map(_.buildModel(builders)))

  def createBuilders(tablesByQName: Map[MQName, TableBuilder]) = new Builders(tablesByQName)

  class Builders(val tablesByQName: Map[MQName, TableBuilder])

  /** Converts from java.sql.Types w/ type name to the corresponding Java class name (with fully qualified path). */
  def jdbcTypeToScala(jdbcType: Int, typeName: String = ""): ClassTag[?] = {
    import java.sql.Types.*

    import scala.reflect.classTag
    // see TABLE B-1 of JSR-000221 JBDCTM API Specification 4.1 Maintenance Release
    // Mapping to corresponding Scala types where applicable
    jdbcType match {
      case CHAR | VARCHAR | LONGVARCHAR | NCHAR | NVARCHAR | LONGNVARCHAR => classTag[String]
      case NUMERIC | DECIMAL => classTag[BigDecimal]
      case BIT | BOOLEAN => classTag[Boolean]
      case TINYINT => classTag[Byte]
      case SMALLINT => classTag[Short]
      case INTEGER => classTag[Int]
      case BIGINT => classTag[Long]
      case REAL => classTag[Float]
      case FLOAT | DOUBLE => classTag[Double]
      case BINARY | VARBINARY | LONGVARBINARY | BLOB => classTag[java.sql.Blob]
      case DATE => classTag[java.sql.Date]
      case TIME => classTag[java.sql.Time]
      case TIMESTAMP => classTag[java.sql.Timestamp]
      case CLOB => classTag[java.sql.Clob]
      // case ARRAY => classTag[java.sql.Array]
      // case STRUCT => classTag[java.sql.Struct]
      // case REF => classTag[java.sql.Ref]
      // case DATALINK => classTag[java.net.URL]
      // case ROWID => classTag[java.sql.RowId]
      // case NCLOB => classTag[java.sql.NClob]
      // case SQLXML => classTag[java.sql.SQLXML]
      case NULL => ClassTag.Null
      case DISTINCT => logger.warn(s"Found jdbc type DISTINCT. Assuming Blob. This may be wrong. You can override ModelBuilder#Table#Column#tpe to fix this."); classTag[java.sql.Blob] // FIXME
      case t => logger.warn(s"Found unknown jdbc type $t. Assuming String. This may be wrong. You can override ModelBuilder#Table#Column#tpe to fix this."); classTag[String] // FIXME
    }
  }

  /** The Scala source name of a ClassTag's class. Unlike `ClassTag.toString`, which uses the binary name, a nested
    * class such as `Outer.Inner` (binary name `Outer$Inner`, where `Outer$` is the class of a Scala object) is
    * rendered with dots. Other classes are rendered as by `ClassTag.toString`. */
  private def scalaTypeName(ct: ClassTag[?]): String = {
    def sourceName(c: Class[?]): String =
      if(c.isArray) s"Array[${sourceName(c.getComponentType)}]"
      else c.getDeclaringClass match {
        case null => c.getName
        case d => sourceName(d).stripSuffix("$") + "." + c.getName.stripPrefix(d.getName + "$")
      }
    val c = ct.runtimeClass
    if(c.isArray || c.getDeclaringClass != null) sourceName(c) else ct.toString
  }

  ///////////////////////////////////////////////////////////////////////////////////////////// Builder implementations

  /** Determines the qualified name of a table in the model.
    * @group Basic customization overrides */
  class TableNamer(val meta: MTable) {
    /** Table name */
    def name: String = meta.name.name
    /** Optional table schema
      * @group Basic customization overrides */
    def schema: Option[String] = meta.name.schema
    /** Optional table catalog
      * @group Basic customization overrides */
    def catalog = meta.name.catalog
    /** Fully qualified table name */
    final lazy val qualifiedName = m.QualifiedName(name,schema,catalog)
  }

  /** Table model builder
    * @group Basic customization overrides */
  class TableBuilder(val meta: MTable,
                     val namer: TableNamer,
                     val mColumns: Seq[MColumn],
                     val mPrimaryKeys: Seq[MPrimaryKey],
                     val mForeignKeys: Seq[Seq[MForeignKey]],
                     val mIndices: Seq[Seq[MIndexInfo]]) { table =>

    // models
    def buildModel(builders: Builders) = m.Table(namer.qualifiedName, columns, primaryKey, buildForeignKeys(builders), indices)
    /** Column models in ordinal position order */
    final lazy val columns: Seq[m.Column] = mColumns.map(c => createColumnBuilder(this, c).model)
    /** Column models by name */
    final lazy val columnsByName: Map[String,m.Column] = columns.map(c => c.name -> c).toMap
    /** Primary key models in key sequence order */
    final lazy val primaryKey: Option[m.PrimaryKey] = createPrimaryKeyBuilder(this, mPrimaryKeys).model
    /** Foreign key models by key sequence order */
    final def buildForeignKeys(builders: Builders) =
      mForeignKeys.map(mf => createForeignKeyBuilder(this, mf).buildModel(builders)).flatten
    /** Index models by ordinal position order */
    final lazy val indices: Seq[m.Index] = mIndices.map(mi => createIndexBuilder(this, mi).model).flatten
  }

  /** Column model builder.
    * @group Basic customization overrides */
  class ColumnBuilder(tableBuilder: TableBuilder, meta: MColumn) {
    /** Regex matcher to extract string out ouf surrounding '' */
    final val StringPattern = """^'(.*)'$""".r
    /** Matches a default that is the current date or time, with an optional fractional-seconds precision, such
      * as MySQL's `CURRENT_TIMESTAMP(6)` */
    private val CurrentTimePattern = """(?i)(NOW|CURRENT_TIMESTAMP|CURRENT_DATE|CURRENT_TIME)(\(\d*\))?""".r
    /** Scala type this column is mapped to */
    def tpe = scalaTypeName(jdbcTypeToScala(meta.sqlType, meta.typeName)) match {
      case "java.lang.String" => if(meta.size == Some(1)) "Char" else "String"
      case t => t
    }
    /** Column name */
    def name = meta.name
    /** Indicates whether this is a nullable column */
    def nullable = meta.nullable.getOrElse(true)
    /** Indicates whether this is an auto increment column */
    def autoInc: Boolean = meta.isAutoInc.getOrElse(false)
    /** Indicates whether this is a generated column */
    def generated: Boolean = meta.isGenerated.getOrElse(false)
    /** Indicates whether a ColumnOption Primary key should be put into the model.
      * Only valid for single column primary keys. */
    def createPrimaryKeyColumnOption: Boolean =
      tableBuilder.mPrimaryKeys.size == 1 && tableBuilder.mPrimaryKeys.head.column == meta.name
    /** A (potentially non-portable) database column type. For string types, this should not
      * include a length ascription. */
    def dbType: Option[String] = Some(meta.typeName)
    /** Column length of string types */
    def length: Option[Int] = if(tpe == "String") meta.size else None // Only valid for strings!
    /** Indicates whether this should be a varchar in case of a string column.
      * Should be based on the value of dbType in the future. */
    def varying: Boolean = {
      import java.sql.Types.*
      Seq(NVARCHAR, VARCHAR, LONGVARCHAR, LONGNVARCHAR) contains meta.sqlType
    }

    /** The default value as reported by the JDBC driver (unparsed SQL), if any */
    def rawDefault = meta.columnDef

    /** The default value for the column. The outer option is used to indicate if a default value is given. The inner
      * Option is used to allow giving None for a nullable column. This method must not return Some(None) for a
      * non-nullable column.
      *
      * Default values for autoInc column are automatically ignored (as if returning None).
      *
      * If `ignoreInvalidDefaults = true`, Slick catches scala.MatchError and java.lang.NumberFormatException thrown by
      * this method, logs the message and treats it as no default value for convenience. */
    def default: Option[Option[Any]] = rawDefault.map { v =>
      if(v == "NULL") None else {
        // NOTE: When extending this list, please also extend the code generator accordingly
        Some(((v,tpe): @unchecked) match {
          case (v,"Byte")   => v.toByte
          case (v,"Short")  => v.toShort
          case (v,"Int")    => v.toInt
          case (v,"Long")   => v.toLong
          case (v,"Double") => v.toDouble
          case (v,"Float")  => v.toFloat
          case (v,"Char")   =>
            v.length match {
              case 1 => v(0)
              case 3 => v(1) // quoted character
            }
          case (v,"String") if meta.typeName == "CHAR" => v.head // FIXME: check length
          case (v,"scala.math.BigDecimal") => BigDecimal(s"${v.trim}") // need the trim for Oracle trailing space
          case (StringPattern(str),"String") => str
          case ("TRUE","Boolean")  => true
          case ("FALSE","Boolean") => false
          // Intentionally non-exhaustive: unrecognised (value, type) pairs throw MatchError,
          // which is caught and logged when ignoreInvalidDefaults = true.
        })
      }
    }

    private def formatDefault(v:Any) =
      s" default value $v for column ${tableBuilder.namer.qualifiedName.asString}.$name of type $tpe, meta data: "+meta.toString

    /** The default value for the column as a ColumnOption Default or None if no default. The value wrapped by
      * ColumnOption Default needs to be an Option in case of a nullable column but can't be an Option in case of a
      * non-nullable Column.
      *
      * Default values for autoInc columns are automatically ignored.
      *
      * If `ignoreInvalidDefaults = true`, Slick catches scala.MatchError and java.lang.NumberFormatException thrown by
      * this method, logs the message and treats it as no default value for convenience. */
    def defaultColumnOption: Option[RelationalProfile.ColumnOption.Default[?]] = rawDefault.map(v => (v,tpe)).collect {
      case (v @ CurrentTimePattern(_, _), _) =>
        logger.debug(s"Ignoring"+formatDefault(v))
        None
    }.getOrElse {
      default.map( d =>
        RelationalProfile.ColumnOption.Default(
          if(nullable) d
          else d.getOrElse(throw new SlickException(s"Invalid default value $d for non-nullable column ${tableBuilder.namer.qualifiedName.asString}.$name of type $tpe, meta data: "+meta.toString))
        )
      )
    }

    private def convenientDefault: Option[RelationalProfile.ColumnOption.Default[?]] =
      try defaultColumnOption catch {
        case e: java.lang.NumberFormatException if ignoreInvalidDefaults =>
          logger.debug(s"NumberFormatException: Could not parse"+formatDefault(rawDefault))
          None
        case e: scala.MatchError =>
          val msg = "Could not parse" + formatDefault(rawDefault)
          if(ignoreInvalidDefaults) {
            logger.debug(s"SlickException: $msg")
            None
          } else throw new SlickException(msg, e)
      }

    def model = m.Column(name=name, table=tableBuilder.namer.qualifiedName, tpe=tpe, nullable=nullable,
      options = Set() ++
        dbType.map(str => SqlProfile.ColumnOption.SqlType(str)) ++
        (if(autoInc || generated) Some(ColumnOption.AutoInc) else None) ++
        (if(createPrimaryKeyColumnOption) Some(ColumnOption.PrimaryKey) else None) ++
        length.map(RelationalProfile.ColumnOption.Length.apply(_,varying=varying)) ++
        (if(!autoInc && !generated) convenientDefault else None) )
  }

  /** Primary key model builder.
    * @group Basic customization overrides */
  class PrimaryKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MPrimaryKey]){
    /** Indicates wether a primary key should be generated. Disabled by default for single column primary keys in favor
      * of ColumnOption PrimaryKey via Column#createPrimaryKeyColumnOption. */
    def enabled: Boolean = meta.size > 1
    /** Primary key name, if the database reports one */
    def name: Option[String] = meta.head.pkName.filter(_ != "")
    /** Names of the primary key columns in key sequence order */
    def columns = meta.map(_.column)
    // single column primary keys excluded in favor of PrimaryKey column option
    final def model: Option[m.PrimaryKey] = if(!enabled) None else Some(
      m.PrimaryKey(name, tableBuilder.namer.qualifiedName,columns.map(tableBuilder.columnsByName))
    )
  }

  /** Foreign key model builder.
    * @group Basic customization overrides */
  class ForeignKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MForeignKey]) {
    private val fk = meta.head
    /** Indicates whether a foreign key should be generated. Enabled by default. */
    def enabled: Boolean = true
    /** Foreign key name, if the database reports one */
    def name: Option[String] = fk.fkName.filter(_ != "")
    /** Names of the columns in this table that hold the foreign key, in key sequence order */
    def referencedColumns  = meta.map(_.fkColumn)
    private val referencingColumns = meta.map(_.pkColumn)
    assert(referencingColumns.size == referencedColumns.size)
    /** Action taken when the referenced row is updated */
    def updateRule: m.ForeignKeyAction = fk.updateRule
    /** Action taken when the referenced row is deleted */
    def deleteRule: m.ForeignKeyAction = fk.deleteRule

    final def buildModel(builders: Builders): Option[m.ForeignKey] = {
      assert(meta.size >= 1)
      assert(tableBuilder.namer.qualifiedName == tableNamersByQName(fk.fkTable).qualifiedName)
      if(!enabled) None else Some(m.ForeignKey(
        name,
        tableBuilder.namer.qualifiedName,
        referencedColumns.map(tableBuilder.columnsByName),
        tableNamersByQName(fk.pkTable).qualifiedName,
        referencingColumns.map(builders.tablesByQName(fk.pkTable).columnsByName),
        updateRule,
        deleteRule
      ))
    }
  }

  /** Index model builder.
    * @group Basic customization overrides */
  class IndexBuilder(tableBuilder: TableBuilder, meta: Seq[MIndexInfo]) {
    private val idx = meta.head
    assert(meta.size >= 1)
    assert(meta.forall(_.indexName == idx.indexName))
    assert(meta.forall(_.nonUnique == idx.nonUnique))
    /** Indicates wether an index should be generated. Disabled by default for:
      * - indexType == tableIndexStatistic
      * - indices matching primary key
      * - non-unique indices matching foreign keys referencing columns
      * - indices matching foreign keys referenced columns */
    def enabled = (
      idx.indexType != DatabaseMetaData.tableIndexStatistic &&
        (tableBuilder.mPrimaryKeys.isEmpty || tableBuilder.mPrimaryKeys.map(_.column).toSet != columns.toSet) &&
        // preserve additional uniqueness constraints on (usually not unique) fk columns
        (unique || tableBuilder.mForeignKeys.forall(_.map(_.fkColumn).toSet != columns.toSet)) &&
        // postgres may refer to column oid, skipping index for now. Maybe we should generate a column and include it
        // instead. And maybe this should be moved into PostgresModelBuilder.
        // TODO: This needs a test case!
        columns.forall(tableBuilder.columnsByName.isDefinedAt)
      )

    /** Indicates whether this is a unique index */
    def unique = !idx.nonUnique
    /** Names of the indexed columns in ordinal position order */
    def columns = meta.flatMap(_.column)
    /** Index name, if the database reports one */
    def name = idx.indexName.filter(_ != "")
    final def model: Option[m.Index] =
      if(!enabled) None
      else Some(m.Index(name, tableBuilder.namer.qualifiedName, columns.map(tableBuilder.columnsByName), unique))
  }
}
