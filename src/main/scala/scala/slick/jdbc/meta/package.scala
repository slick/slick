package scala.slick.jdbc
import slick.util.SlickLogger
import org.slf4j.LoggerFactory
import scala.reflect.ClassTag
import scala.slick.SlickException
package object meta{
  import scala.slick.driver.JdbcProfile
  import scala.slick.jdbc.JdbcBackend
  import scala.slick.ast.ColumnOption
  /**
   * Creates a Slick data model from jdbc meta data.
   * Foreign keys pointing out of the given tables are not included.
   * @param mTables tables to include in the model
   * @param profile JdbcProfile that was used to retrieve mTables (using a different one can lead to exceptions)
   */
  def createModel(mTables: Seq[MTable], profile: JdbcProfile)(implicit session: JdbcBackend#Session) : slick.model.Model = {
    lazy val logger = new SlickLogger(LoggerFactory.getLogger("scala.slick.jdbc.meta"))
    import java.sql.DatabaseMetaData
    import scala.slick.{model => m}
    import collection.immutable.ListMap
    lazy val mTablesByMQName: Map[MQName,MTable] = mTables.map(t => t.name -> t).toMap
    lazy val mPrimaryKeysByMQName: Map[MQName,Seq[MPrimaryKey]] = mTables.map(t => t.name -> t.getPrimaryKeys.list.sortBy(_.keySeq)).toMap
    val tableNameByMQName = mTables.map(_.name).map( name =>
      name -> m.QualifiedName(
        name.name,
        schema=name.schema
                   // TODO: move these out into the drivers
                   .filter(_ != "PUBLIC") // H2 / Hsqldb default
                   .filter(_ != "public") // Postgres default
                   .filter(_ != "APP"), // Derby default                 
        name.catalog
      )
    ).toMap

    val columnsByTableAndName: Map[MQName,Map[String,m.Column]] = {
      def column(tableName: m.QualifiedName, column: MColumn) = {
        val mPrimaryKeys = mPrimaryKeysByMQName(column.table)
        val IntValue = "^([0-9]*)$".r
        val DoubleValue = "^([0-9*]\\.[0-9]*)$".r
        val StringValue = """^'(.+)'$""".r
        import ColumnOption._
        val tpe = jdbcTypeToScala(column.sqlType).toString match {
          case "Object" => "AnyRef"
          case "java.lang.String" => "String"
          case t => t
        }
        val nullable = column.nullable.getOrElse(true)
        val autoInc = column.isAutoInc.getOrElse(false)
        val c = m.Column(
          name=column.name,
          table=tableName,
          tpe=tpe,
          nullable=nullable,
          // omitting the DBType as it is not portable between backends
          options = Set() ++
            (if(autoInc) Some(AutoInc) else None) ++
            (column.columnDef.filter(_ => !autoInc).flatMap( v =>
              try{
                Some((v,tpe) match {
                  // NOTE: When extending this list, please also extend the code generator accordingly
                  case (_,"Int") => v.toInt
                  case (_,"Long") => v.toLong
                  case (_,"Short") => v.toShort // seen in Derby
                  case (_,"Float") => v.toFloat
                  case (_,"Double") => v.toDouble
                  case (StringValue(str),"String") => str
                  case ("NULL",_) if nullable => None
                  //case (_,"String") => v // seen in MySQL // buggy in postgres, found value 'unchecked'::character varying
                  case ("1","Boolean") => true // seen in MySQL
                  case ("0","Boolean") => false
                  case ("true","Boolean") => true // seen in postgres
                  case ("false","Boolean") => false
                  case ("TRUE","Boolean") => true // seen in H2
                  case ("FALSE","Boolean") => false
                  case ("CURRENT_TIMESTAMP","java.sql.Timestamp") => throw new SlickException(s"Ignoring default value CURRENT_TIMESTAMP of column $tableName.${column.name} of type $tpe")
                  case _ => throw new SlickException(s"Could not parse default value $v of column $tableName.${column.name} of type $tpe")
                })
              } catch {
                case e: java.lang.NumberFormatException => logger.debug(s"NumberFormatException: Could not parse default value $v of column $tableName.${column.name} as $tpe"); None
                case e: SlickException => logger.debug(e.getMessage); None
              }
             ).map(Default.apply)) ++
            // Add ColumnOption if single column primary key
            (if(mPrimaryKeys.size == 1) mPrimaryKeys.filter(_.column == column.name).map(_ => PrimaryKey) else Set())
        )
        c
      }

      mTablesByMQName.mapValues( t => ListMap(t.getColumns.list.sortBy(_.ordinalPosition).map(c => c.name -> column(tableNameByMQName(t.name),c)):_*))
    }

    def table(mTable: MTable) = {
      val tableName = tableNameByMQName(mTable.name)
      val columns = columnsByTableAndName(mTable.name).values.toSeq
      val columnsByName: Map[String,m.Column] = columns.map(c => c.name -> c).toMap
      
      def primaryKey(mPrimaryKeys:Seq[MPrimaryKey]) = {
        // single column primary keys excluded in favor of PrimaryKey column option
        if(mPrimaryKeys.size <= 1) None else Some(
          m.PrimaryKey(
            mPrimaryKeys.head.pkName.filter(_ != "")/*MySQL workaround:*/.filter(_ != "PRIMARY"),
            tableName,
            mPrimaryKeys.map(_.column).map(columnsByName)
          )
        )
      }
      def foreignKeys(mForeignKeys:Seq[MForeignKey]) = {
        mForeignKeys
          // remove foreign keys pointing to tables which were not included
          .filter(fk => mTablesByMQName.isDefinedAt(fk.pkTable))
          .groupBy(fk => (fk.pkTable,fk.fkName,fk.pkName,fk.fkTable))
          .toSeq
          .sortBy{case (key,_) => (key._1.name,key._2,key._3,key._4.name)}
          .map(_._2.sortBy(_.keySeq)) // respect order
          .map{ fks =>
            val fk = fks.head
            assert(tableName == tableNameByMQName(fk.fkTable))
            val fkColumns = fks.map(_.fkColumn).map(columnsByName)
            val pkColumns = fks.map(_.pkColumn).map(columnsByTableAndName(fk.pkTable))
            assert(fkColumns.size == pkColumns.size)
            m.ForeignKey(
              fk.fkName.filter(_ != ""),
              tableName,
              fkColumns,
              tableNameByMQName(fk.pkTable),
              pkColumns,
              fk.updateRule,
              fk.deleteRule
            )
          }
      }
      def indices(mIndexInfo: Seq[MIndexInfo]) = {
        mIndexInfo
          // filter out unnecessary tableIndexStatistic (we can safely call .get later)
          .filter(_.indexType != DatabaseMetaData.tableIndexStatistic)
          .groupBy(_.indexName)
          .toSeq
          .sortBy(_._1)
          .map(_._2.sortBy(_.ordinalPosition)) // respect order
          .map{ mIndices =>
            val idx = mIndices.head
            m.Index(
              idx.indexName.filter(_ != ""),
              tableName,
              mIndices.map(
                _.column.get.stripPrefix("\"").stripSuffix("\"") // strip " to work around postgres issue
              ).map(columnsByName),
              !idx.nonUnique
            )
          }          
      }

      val mPrimaryKeys = mPrimaryKeysByMQName(mTable.name)
      val fks = foreignKeys(mTable.getImportedKeys.list)
      m.Table(
        tableName,
        columns,
        primaryKey(mPrimaryKeys),
        fks,
        // indices not including primary key and table statistics
        indices(mTable.getIndexInfo().list)
          .filter{ 
            // filter out foreign key index
            case idx if !idx.unique => !fks.exists(_.referencingColumns.toSet == idx.columns.toSet)
            // filter out primary key index
            case idx  => mPrimaryKeys.isEmpty || mPrimaryKeys.map(_.column).toSeq != idx.columns.map(_.name).toSeq
          }
      )
    }

    m.Model( mTables.sortBy(_.name.name).map(table) )
  }

  /** Converts from java.sql.Types to the corresponding Java class name (with fully qualified path). */
  def jdbcTypeToScala(jdbcType: Int): ClassTag[_] = {
    lazy val logger = new SlickLogger(LoggerFactory.getLogger("scala.slick.jdbc.meta"))
    import java.sql.Types._
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
      case NULL => classTag[Null]
      case DISTINCT => logger.warn(s"Found jdbc type DISTINCT. Assuming Blob. This may be wrong."); classTag[java.sql.Blob] // FIXME
      case t => logger.warn(s"Found unknown jdbc type $t. Assuming String. This may be wrong."); classTag[String] // FIXME
    }
  }
}
