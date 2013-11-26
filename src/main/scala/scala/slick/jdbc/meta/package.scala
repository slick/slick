package scala.slick.jdbc
package object meta{
  import scala.slick.driver.JdbcProfile
  import scala.slick.jdbc.JdbcBackend
  /**
   * Creates a Slick meta model from jdbc meta data.
   * Foreign keys pointing out of the given tables are not included.
   * @param mTables tables to include in the model
   * @param profile JdbcProfile that was used to retrieve mTables (using a different one can lead to exceptions)
   */
  def createMetaModel(mTables: Seq[MTable], profile: JdbcProfile)(implicit session: JdbcBackend#Session) : slick.meta.Model = {
    import java.sql.DatabaseMetaData
    import scala.slick.{meta => m}
    import collection.immutable.ListMap
    lazy val mTablesByMQName: Map[MQName,MTable] = mTables.map(t => t.name -> t).toMap

    val tableNameByMQName = mTables.map(_.name).map( name =>
      name -> m.QualifiedName(name.name, schema=name.schema, name.catalog)
    ).toMap

    val columnsByTableAndName: Map[MQName,Map[String,m.Column]] = {
      val IntValue = "^([0-9]*)$".r
      val DoubleValue = "^([0-9*]\\.[0-9]*)$".r
      val StringValue = """^'(.+)'$""".r
      def column(tableName: m.QualifiedName, column: MColumn) = {
        val c = m.Column(
          column.name,
          tableName,
          column.sqlType,
          column.typeName,
          if(
            !profile.capabilities.contains(JdbcProfile.capabilities.columnSizeMetaData)
            || (
              // FIXME: place this somewhere more appropriate AND create a thorough list of which driver supports size for which types
              // see
              // http://db.apache.org/derby/docs/10.2/ref/
              // http://www.hsqldb.org/doc/1.8/guide/ch09.html#datatypes-section
              // http://hsqldb.org/doc/guide/sqlgeneral-chapt.html#sgc_types_ops
              //(profile == scala.slick.driver.HsqldbDriver || profile == scala.slick.driver.DerbyDriver) && 
              Seq("BOOLEAN","TINYINT","SMALLINT","INTEGER","BIGINT","NUMERIC","DECIMAL","DATE","TIME","DATETIME","TIMESTAMP","DOUBLE","FLOAT","BLOB")
                .contains(column.typeName)
            )
          ){
            None
          } else {
            column.columnSize
          },
          column.nullable.getOrElse(true),
          column.isAutoInc.getOrElse(false),
          column.columnDef.collect{
            case IntValue(value) => Some(value.toInt)
            case DoubleValue(value) => Some(value.toDouble)
            case StringValue(value) => Some(value)
            case "NULL" => None
          }
        )
        c
      }

      mTablesByMQName.mapValues( t => ListMap(t.getColumns.list.map(c => c.name -> column(tableNameByMQName(t.name),c)):_*))
    }

    def table(mTable: MTable) = {
      val tableName = tableNameByMQName(mTable.name)
      val columns = columnsByTableAndName(mTable.name).values.toSeq
      val columnsByName: Map[String,m.Column] = columns.map(c => c.name -> c).toMap
      
      def primaryKey(mPrimaryKeys:Seq[MPrimaryKey]) = {
        if(mPrimaryKeys.isEmpty) None else Some(
          m.PrimaryKey(
            mPrimaryKeys.head.pkName.getOrElse(""),
            tableName,
            mPrimaryKeys.sortBy(_.keySeq).map(_.column).map(columnsByName)
          )
        )
      }
      def foreignKeys(mForeignKeys:Seq[MForeignKey]) = {
        mForeignKeys
          // remove foreign keys pointing to tables which were not included
          .filter(fk => mTablesByMQName.isDefinedAt(fk.pkTable))
          .groupBy(fk => (fk.fkName,fk.pkName,fk.pkTable,fk.fkTable))
          .values
          .map(_.sortBy(_.keySeq)) // respect order
          .map{ fks =>
            val fk = fks.head
            assert(tableName == tableNameByMQName(fk.fkTable))
            val fkColumns = fks.map(_.fkColumn).map( columnsByName )
            val pkColumns = fks.map(_.pkColumn).map( columnsByTableAndName(fk.pkTable) )
            assert(fkColumns.size == pkColumns.size)
            m.ForeignKey(
              fk.fkName.getOrElse(""),
              tableName,
              fkColumns,
              tableNameByMQName(fk.pkTable),
              pkColumns,
              fk.updateRule,
              fk.deleteRule
            )
          }
          .toSeq
      }
      def indices(mIndexInfo: Seq[MIndexInfo]) = {
        mIndexInfo
          // filter out unnecessary tableIndexStatistic (we can safely call .get later)
          .filter(_.indexType != DatabaseMetaData.tableIndexStatistic)
          .groupBy(_.indexName)
          .values
          .map{ mIndices =>
            val idx = mIndices.head
            m.Index(
              idx.indexName.getOrElse(""),
              tableName,
              mIndices.sortBy(_.ordinalPosition).map(_.column.get).map(columnsByName),
              !idx.nonUnique
            )
          }
          .toSeq
      }

      val pk = primaryKey(mTable.getPrimaryKeys.list)
      val fks = foreignKeys(mTable.getImportedKeys.list)
      m.Table(
        tableName,
        columns,
        pk,
        fks,
        // indices not including primary key and table statistics
        indices(mTable.getIndexInfo().list)
          .filter{ 
            // filter out foreign key index
            case idx if !idx.unique => !fks.exists(_.referencingColumns.toSet == idx.columns.toSet)
            // filter out primary key index
            case idx  => pk.isEmpty || pk.get.columns.toSet != idx.columns.toSet
          }
      )
    }

    m.Model( mTables.map(table) )
  }
}
