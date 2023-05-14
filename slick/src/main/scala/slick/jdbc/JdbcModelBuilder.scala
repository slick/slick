package slick.jdbc


import java.sql.DatabaseMetaData

import scala.concurrent.ExecutionContext
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
class JdbcModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends Logging {

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

  def createTableNamer(meta: MTable): TableNamer = new TableNamer(meta)

  /** Column model builder factory. Override for customization.
    * @group Basic customization overrides */
  def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder = new ColumnBuilder(tableBuilder, meta)

  def createPrimaryKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MPrimaryKey]): PrimaryKeyBuilder = new PrimaryKeyBuilder(tableBuilder, meta)

  def createForeignKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MForeignKey]): ForeignKeyBuilder = new ForeignKeyBuilder(tableBuilder, meta)

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
  def jdbcTypeToScala(jdbcType: Int, typeName: String = ""): ClassTag[_] = {
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

  ///////////////////////////////////////////////////////////////////////////////////////////// Builder implementations

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
    /** Scala type this column is mapped to */
    def tpe = jdbcTypeToScala(meta.sqlType, meta.typeName).toString match {
      case "java.lang.String" => if(meta.size == Some(1)) "Char" else "String"
      case t => t
    }
    def name = meta.name
    /** Indicates whether this is a nullable column */
    def nullable = meta.nullable.getOrElse(true)
    /** Indicates whether this is an auto increment column */
    def autoInc: Boolean = meta.isAutoInc.getOrElse(false)
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
        Some((v,tpe) match {
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
    def defaultColumnOption: Option[RelationalProfile.ColumnOption.Default[_]] = rawDefault.map(v => (v,tpe)).collect {
      case (v,_) if Seq("NOW","CURRENT_TIMESTAMP","CURRENT_DATE","CURRENT_TIME").contains(v.stripSuffix("()").toUpperCase) =>
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

    private def convenientDefault: Option[RelationalProfile.ColumnOption.Default[_]] =
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
        (if(autoInc) Some(ColumnOption.AutoInc) else None) ++
        (if(createPrimaryKeyColumnOption) Some(ColumnOption.PrimaryKey) else None) ++
        length.map(RelationalProfile.ColumnOption.Length.apply(_,varying=varying)) ++
        (if(!autoInc) convenientDefault else None) )
  }

  class PrimaryKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MPrimaryKey]){
    /** Indicates wether a primary key should be generated. Disabled by default for single column primary keys in favor
      * of ColumnOption PrimaryKey via Column#createPrimaryKeyColumnOption. */
    def enabled: Boolean = meta.size > 1
    def name: Option[String] = meta.head.pkName.filter(_ != "")
    def columns = meta.map(_.column)
    // single column primary keys excluded in favor of PrimaryKey column option
    final def model: Option[m.PrimaryKey] = if(!enabled) None else Some(
      m.PrimaryKey(name, tableBuilder.namer.qualifiedName,columns.map(tableBuilder.columnsByName))
    )
  }

  class ForeignKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MForeignKey]) {
    private val fk = meta.head
    def enabled: Boolean = true
    def name: Option[String] = fk.fkName.filter(_ != "")
    def referencedColumns  = meta.map(_.fkColumn)
    private val referencingColumns = meta.map(_.pkColumn)
    assert(referencingColumns.size == referencedColumns.size)
    def updateRule: m.ForeignKeyAction = fk.updateRule
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

    def unique = !idx.nonUnique
    def columns = meta.flatMap(_.column)
    def name = idx.indexName.filter(_ != "")
    final def model: Option[m.Index] =
      if(!enabled) None
      else Some(m.Index(name, tableBuilder.namer.qualifiedName, columns.map(tableBuilder.columnsByName), unique))
  }
}
