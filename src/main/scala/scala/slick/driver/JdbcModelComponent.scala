package scala.slick.driver

import scala.slick.jdbc.meta._
import scala.slick.jdbc._
import slick.util.SlickLogger
import org.slf4j.LoggerFactory
import scala.reflect.ClassTag
import scala.slick.SlickException
import scala.slick.ast.ColumnOption
import java.sql.DatabaseMetaData
import scala.slick.model.Model

trait JdbcModelComponent{ driver: JdbcDriver =>
    @deprecated("use defaultTables instead","2.1")
  final def getTables: Invoker[MTable] = MTable.getTables

  /** Jdbc meta data for all tables included in the Slick model by default */
  def defaultTables(implicit session: Backend#Session): Seq[MTable] = MTable.getTables.list

  @deprecated("use createModel() instead (appending parenthesis)","2.1")
  def createModel(implicit session: Backend#Session): Model = createModel(Some(defaultTables))

  /** Gets the Slick data model describing this data source
    * @param tables used to build the model, uses defaultTables if None given
    * @param ignoreInvalidDefaults logs unrecognized default values instead of throwing an exception
    */
  def createModel(tables: Option[Seq[MTable]] = None, ignoreInvalidDefaults: Boolean = true)
                          (implicit session: Backend#Session)
                          : Model
    = new ModelBuilder(tables.getOrElse(defaultTables), ignoreInvalidDefaults){}.model

  /** Build a Slick model from introspecting the jdbc meta data.
      Similar to SlickCodeGenerator, this is implemented as nested classes,
      which allow hooking into the behavior by overriding methods. This comes at the price
      of very tight coupling. In most cases you are better of transforming
      the generated model instead of overriding functionality here.
      It is only useful, if you need easy access
      to the jdbc meta data in order to influence how the model is generated.
      A good use case would be interpreting column types or default values Slick
      doesn't understand out of the box. If you just want to remove or hard code
      some default values, just transform the resulting model instead.

      The tight coupling can easily lead to sorce code incompatibilities in
      future versions. Avoid hooking in here if you don't have to.

      @param ignoreInvalidDefaults see ModelBuilder#Table#Column#default
      */
  class ModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit session: JdbcBackend#Session){
    import scala.slick.{model => m}

    /** workaround for https://issues.scala-lang.org/browse/SI-7769
        sets default value for ignoreInvalidDefaults to true */
    def this(mTables: Seq[MTable])(implicit session: JdbcBackend#Session) = this(mTables,true)
    final lazy val logger = new SlickLogger(LoggerFactory.getLogger("scala.slick.jdbc.meta"))

    /** Table model builders */
    final lazy val tables: Seq[Table] = mTables.map(Table)
    /** Table model builders indexed by meta data qualified name */
    final lazy val tablesByMQName: Map[MQName,Table] = tables.map(t => t.meta.name -> t).toMap

    /** currently unused */
    def includeJdbcMetaData: Boolean = ???

    /** Table model builder factory. Override for customization.
        @group Basic customization overrides */
    def Table = new Table(_)
    /** Table model builder
        @group Basic customization overrides */
    class Table(val meta: MTable){
      table =>
      // meta data
      /** cached primary key meta data and in key sequence order */
      final lazy val mPrimaryKeys: Seq[MPrimaryKey]
        = meta.getPrimaryKeys.list.sortBy(_.keySeq)
      /** cached foreign key meta data grouped by name and in key sequence order */
      final lazy val mForeignKeys: Seq[Seq[MForeignKey]]
        = meta.getImportedKeys.list
              // remove foreign keys pointing to tables which were not included
              .filter(fk => tablesByMQName.isDefinedAt(fk.pkTable))
              .groupBy(fk => (fk.pkTable,fk.fkName,fk.pkName,fk.fkTable))
              .toSeq
              .sortBy{case (key,_) => (key._1.name,key._2,key._3,key._4.name)}
              .map(_._2.sortBy(_.keySeq)) // respect order
      /** cached index meta data grouped by name and in ordinal position order */
      final lazy val mIndices
        = try{
          meta.getIndexInfo().list
              .groupBy(_.indexName)
              .toSeq
              .sortBy(_._1)
              .map(_._2.sortBy(_.ordinalPosition)) // respect order
        } catch {
          case e:java.sql.SQLException => // TODO: this needs a test!
            logger.debug(s"Skipping indices of table ${meta.name.name} due to exception during getIndexInfo: "+e.getMessage.trim)
            Seq()
        }

      // models
      
      def model = m.Table(
        qualifiedName,
        columns,
        primaryKey,
        foreignKeys,
        indices
      )
      /** Column models in ordinal position order */
      final lazy val columns: Seq[m.Column]
        = meta.getColumns
              .list
              .sortBy(_.ordinalPosition)
              .map(c => Column(c).model)
      /** Column models by name */
      final lazy val columnsByName: Map[String,m.Column]
        = columns.map(c => c.name -> c).toMap
      /** Primary key models in key sequence order */
      final lazy val primaryKey: Option[m.PrimaryKey]
        = PrimaryKey(mPrimaryKeys).model
      /** Foreign key models by key sequence order */
      final lazy val foreignKeys: Seq[m.ForeignKey]
        = mForeignKeys.map(ForeignKey(_).model).flatten
      /** Index models by ordinal position order */
      final lazy val indices: Seq[m.Index]
        = mIndices.map(Index(_).model).flatten

      /** Table name */
      def name: String = meta.name.name
      /** Optional table schema
          @group Basic customization overrides */
      def schema: Option[String] = meta.name.schema
      /** Optional table catalog
          @group Basic customization overrides */
      def catalog = meta.name.catalog
      /** Fully qualified table name */
      final lazy val qualifiedName = m.QualifiedName(name,schema,catalog)
      
      /** Column model builder factory. Override for customization.
          @group Basic customization overrides */
      def Column = new Column(_)
      /** Column model builder factory.
          @group Basic customization overrides */
      class Column(val meta: MColumn){
        /** Regex matcher to extract string out ouf surrounding '' */
        final val StringPattern = """^'(.*)'$""".r
        /** Scala type this column is mapped to */
        def tpe = jdbcTypeToScala(meta.sqlType).toString match {
          case "java.lang.String" => "String"
          case t => t
        }
        def name = meta.name
        /** Indicates whether this is a nullable column */
        def nullable = meta.nullable.getOrElse(true)
        /** Indicates whether this is an auto increment column */
        def autoInc: Boolean = meta.isAutoInc.getOrElse(false)
        /** Indicates whether a ColumnOption Primary key should be put into the model.
            Only valid for single column primary keys.
            */
        def createPrimaryKeyColumnOption: Boolean =
          mPrimaryKeys.size == 1 && mPrimaryKeys.head.column == meta.name
        /** A (potentially non-portable) database column type
          * for string types, this should not include a length ascription
          * for other types it should */
        def dbType: Option[String] = Some(meta.typeName)
        /** currently unused */
        def ansiType: Option[String] = ???
        /** Column length of string types */
        def length: Option[Int] = if(tpe == "String") meta.size else None // Only valid for strings!
        /** Indicates wether this should be a varchar in case of a string column.
            Currently defaults to true. Should be based on the value of dbType in the future. */
        def varying: Boolean = Seq(
          java.sql.Types.NVARCHAR,
          java.sql.Types.VARCHAR,
          java.sql.Types.LONGVARCHAR,
          java.sql.Types.LONGNVARCHAR
        ) contains meta.sqlType

        /** The default value for the column.
          * The outer option is used to indicate if a default value is given.
          * The inner Option is used to allow giving None for a nullable column.
          * This method must not return Some(None) for a non-nullable column.
          *
          * Default values for autoInc column are automatically ignored (as if returning None).
          *
          * If ignoreInvalidDefaults=true, Slick catches scala.MatchError and
          * java.lang.NumberFormatException thrown by this method,
          * logs the message and treats it as no default value for convenience.
          */

        def default: Option[Option[Any]]
          = meta.columnDef.map{ v =>
            if(v=="NULL"){
              None
            } else {
              // NOTE: When extending this list, please also extend the code generator accordingly
              Some((v,tpe) match {
                case (v,"Byte")   => v.toByte
                case (v,"Short")  => v.toShort
                case (v,"Int")    => v.toInt
                case (v,"Long")   => v.toLong
                case (v,"Double") => v.toDouble
                case (v,"Float") => v.toFloat
                case (v,"Char")   => v.head // FIXME: check length
                case (v,"String") if meta.typeName == "CHAR" => v.head // FIXME: check length
                case (v,"scala.math.BigDecimal") => v // FIXME: probably we shouldn't use a string here
                case (StringPattern(str),"String") => str
                case ("TRUE","Boolean")  => true
                case ("FALSE","Boolean") => false
              })
            }
          }

        private def formatDefault(v:Any) = s" default value $v for column ${table.qualifiedName.asString}.$name of type $tpe, meta data: "+meta.toString

        /** The default value for the column as a ColumnOption Default
          * or None if no default. The value wrapped by
          * ColumnOption Default needs to be an Option in case of a nullable column.
          * But can't be an Option in case of a non-nullable Column.
          *
          * Default values for autoInc columnd are automatically ignored.
          *
          * If ignoreInvalidDefaults=true, Slick catches scala.MatchError and
          * java.lang.NumberFormatException thrown by this method,
          * logs the message and treats it as no default value for convenience.
          */
        def defaultColumnOption: Option[ColumnOption.Default[_]]
          = meta.columnDef.map(v => (v,tpe)).collect{
              case (v@"CURRENT_TIMESTAMP","java.sql.Timestamp") =>
                logger.debug(s"Ignoring"+formatDefault(v))
                None
            }.getOrElse{
              default.map( d =>
                ColumnOption.Default(
                  if(nullable) d
                  else d.getOrElse(throw new SlickException(s"Invalid default value $d for non-nullable column ${table.qualifiedName.asString}.$name of type $tpe, meta data: "+meta.toString))
                )
              )
            }

        private def convenientDefault: Option[ColumnOption.Default[_]] = {
          try{
            defaultColumnOption
          } catch {
            case e: java.lang.NumberFormatException
              if ignoreInvalidDefaults =>
                logger.debug(
                  s"NumberFormatException: Could not parse"+formatDefault(meta.columnDef)
                )
                None
            case e: scala.MatchError =>
              val msg = s"Could not parse"+formatDefault(meta.columnDef)
              if(ignoreInvalidDefaults){
                logger.debug("SlickException: "+msg)
                None
              } else {
                throw new SlickException(msg, e)
              }
          }
        }

        def model = m.Column(
          name=name,
          table=table.qualifiedName,
          tpe=tpe,
          nullable=nullable,
          options =
            Set() ++ 
            dbType.map(ColumnOption.DBType) ++
            (if(autoInc) Some(ColumnOption.AutoInc) else None) ++
            (if(createPrimaryKeyColumnOption) Some(ColumnOption.PrimaryKey) else None) ++
            length.map(ColumnOption.Length.apply(_,varying=varying)) ++
            (if(!autoInc) convenientDefault else None)
        )
      }

      def PrimaryKey = new PrimaryKey(_)
      class PrimaryKey(meta: Seq[MPrimaryKey]){
        /** Indicates wether a primary key should be generated.
            Disabled by default for single column primary keys
            in favor of ColumnOption PrimaryKey via Column#createPrimaryKeyColumnOption. */
        def enabled: Boolean = meta.size > 1
        def name: Option[String] = meta.head.pkName.filter(_ != "")
        def columns = meta.map(_.column)
        // single column primary keys excluded in favor of PrimaryKey column option
        final def model: Option[m.PrimaryKey] = if(!enabled) None else Some(
          m.PrimaryKey(name,table.qualifiedName,columns.map(columnsByName))
        )
      }

      def ForeignKey = new ForeignKey(_)
      class ForeignKey(meta: Seq[MForeignKey]){
        private val fk = meta.head
        assert(meta.size >= 1)
        assert(table.qualifiedName == tablesByMQName(fk.fkTable).qualifiedName)
        def enabled: Boolean = true
        def name: Option[String] = fk.fkName.filter(_ != "")
        def referencedColumns  = meta.map(_.fkColumn)
        private val referencingColumns = meta.map(_.pkColumn)
        assert(referencingColumns.size == referencedColumns.size)
        def updateRule: m.ForeignKeyAction = fk.updateRule
        def deleteRule: m.ForeignKeyAction = fk.deleteRule
        final def model: Option[m.ForeignKey] = if(!enabled) None else Some(m.ForeignKey(
          name,
          table.qualifiedName,
          referencedColumns.map(columnsByName),
          tablesByMQName(fk.pkTable).qualifiedName,
          referencingColumns.map(
            tablesByMQName(fk.pkTable).columnsByName
          ),
          updateRule,
          deleteRule
        ))
      }

      def Index = new Index(_)
      class Index(meta: Seq[MIndexInfo]){
        private val idx = meta.head
        assert(meta.size >= 1)
        assert(meta.forall(_.indexName == idx.indexName))
        assert(meta.forall(_.nonUnique == idx.nonUnique))
        /** Indicates wether an index should be generated.
            Disabled by default for
            - indexType == tableIndexStatistic
            - indices matching primary key
            - non-unique indices matching foreign keys referencing columns
            - indices matching foreign keys referenced columns
            */
        def enabled = (
          idx.indexType != DatabaseMetaData.tableIndexStatistic &&
          (mPrimaryKeys.isEmpty || mPrimaryKeys.map(_.column).toSet != columns.toSet) &&
          // preserve additional uniqueness constraints on (usually not unique) fk columns
          (unique || mForeignKeys.forall(_.map(_.fkColumn).toSet != columns.toSet)) &&
          // postgres may refer to column oid, skipping index for now.
          // Maybe we should generate a column and include it instead.
          // And maybe this should be moved into PostgresModelBuilder
          // TODO: This needs a test case!
          columns.forall(columnsByName.isDefinedAt)
        )

        def unique = !idx.nonUnique
        def columns = meta.flatMap(_.column)
        def name = idx.indexName.filter(_ != "")
        final def model: Option[m.Index]
          = if(!enabled) None else Some(m.Index(
              name,
              table.qualifiedName,
              columns.map(columnsByName),
              unique
            ))
      }
    }

    /**
     * Creates a Slick data model from jdbc meta data.
     * Foreign keys pointing out of the given tables are not included.
     * @param mTables tables to include in the model
     * @param profile JdbcProfile that was used to retrieve mTables (using a different one can lead to exceptions)
     */
    def model: slick.model.Model = {
      m.Model( mTables.sortBy(_.name.name).map(Table(_).model) )
    }

    /** Converts from java.sql.Types to the corresponding Java class name (with fully qualified path). */
    def jdbcTypeToScala(jdbcType: Int): ClassTag[_] = {
      meta.jdbcTypeToScala(jdbcType)
    }
  }
}
