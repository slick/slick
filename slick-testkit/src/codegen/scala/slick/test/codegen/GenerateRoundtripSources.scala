package slick.test.codegen

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import slick.codegen.SourceCodeGenerator
import slick.jdbc.JdbcProfile

/** Generates code for CodeGenRoundTripTest.
  *
  * This is generated using Derby currently because Derby strips column size of some columns, which
  * works with all backends. If the code was generated using model data where the size is included
  * it would fail in derby and hsqldb. The code is tested using all enabled profiles. We should also
  * diversify generation as well at some point. */
object GenerateRoundtripSources {
  def main(args: Array[String]): Unit = {
    val profile = slick.jdbc.H2Profile
    val url = "jdbc:h2:mem:test4"
    val jdbcDriver = "org.h2.Driver"
    object Tables extends Tables(profile)
    import Tables._
    import Tables.profile.api._
    val ddl = posts.schema ++ categories.schema ++ typeTest.schema ++ large.schema ++ `null`.schema ++ X.schema ++ SingleNonOptionColumn.schema ++ SelfRef.schema
    val a1 = profile.createModel(ignoreInvalidDefaults=false).map(m => new SourceCodeGenerator(m) {
      override def tableName = {
        case n if n.toLowerCase == "null" => "null" // testing null as table name
        case n => super.tableName(n)
      }
    })
    val a2 = profile.createModel(ignoreInvalidDefaults=false).map(m => new SourceCodeGenerator(m) {
      override def Table = new Table(_){
        override def autoIncLast = true
        override def Column = new Column(_){
          override def asOption = autoInc
        }
      }
    })
    val db = Database.forURL(url=url, driver=jdbcDriver, keepAliveConnection=true)
    val (gen,gen2) = try Await.result(db.run(ddl.create >> (a1 zip a2)), Duration.Inf) finally db.close
    val pkg = "slick.test.codegen.roundtrip"
    gen.writeToFile( "slick.jdbc.H2Profile", args(0), pkg )
    gen2.writeToFile( "slick.jdbc.H2Profile", args(0), pkg+"2" )
    gen.writeToMultipleFiles( "slick.jdbc.H2Profile", args(0), pkg+"multiplefiles" )
    gen2.writeToMultipleFiles( "slick.jdbc.H2Profile", args(0), pkg+"multiplefiles2" )
  }
}

class Tables(val profile: JdbcProfile){
  import profile.api._
  /** Tests single column table, scala keyword type name, non-dentifier column name and all nullable columns table*/
  class `null`(tag: Tag) extends Table[Option[String]](tag, "null") {
    def name = column[Option[String]]("na me")
    def * = name
  }
  val `null` = TableQuery[`null`]

  /** Tests table with self-referring foreign key */
  class SelfRef(tag: Tag) extends Table[(Int,Option[Int])](tag, "SELF_REF") {
    def id = column[Int]("id",O.PrimaryKey,O.AutoInc)
    def parent = column[Option[Int]]("parent")
    def parentFK = foreignKey("parent_fk", parent, SelfRef)(_.id.?)
    def * = (id,parent)
  }
  val SelfRef = TableQuery[SelfRef]

  /** Tests single column table, scala keyword type name and all nullable columns table*/
  class SingleNonOptionColumn(tag: Tag) extends Table[String](tag, "SingleNonOptionColumn") {
    def name = column[String]("name")
    def * = name
  }
  val SingleNonOptionColumn = TableQuery[SingleNonOptionColumn]

  /** Tests single column table and collision with generated names */
  class all(tag: Tag) extends Table[String](tag, "all") {
    def dynamic = column[String]("dynamic")
    def * = dynamic
  }
  val all = TableQuery[all]

  /** Tests slick term name collision */
  class X(tag: Tag) extends Table[(Int,Int,Option[Int],Int,Double,String,Option[Int],Option[Int],Option[String],Option[String],Option[String])](tag, "X") {
    def pk = column[Int]("pk")
    def pk2 = column[Int]("pk2")
    def pkpk = primaryKey( "primary_key2", (pk,pk2) ) // pk column collision
    def i1 = column[Option[Int]]("index_1") // scala keyword collision
    def c = column[Int]("column") // slick Table method with args collision
    def p = column[Option[Int]]("posts")
    def a = column[Option[Int]]("val") // scala keyword collision
    def s = column[Double]("schema_name") // slick Table no-arg method collision
    def sx = column[String]("schema_name_x") // column name collision after disambiguation
    def t_ag = column[Option[String]]("tag") // column name collision after disambiguation
    def tt = column[Option[String]]("_table_tag") // column name collision after disambiguation
    def _underscore = column[Option[String]]("_underscore") // column name collision after disambiguation
    def * = (pk,pk2,a,c,s,sx,i1,p,t_ag,tt,_underscore)
    def idx1 = index("",i1) // idx column collision
    def idx2 = index("i2",i1) // idx column collision
    def idx3 = index("foo",c,unique=true)
    def idx4 = index("bar",p,unique=true)
    def categoryFK1 = foreignKey("fk1", pk, categories)(_.id) // dup FK collision
    def categoryFK2 = foreignKey("fk2", pk2, categories)(_.id)
    def postsFK = foreignKey("fk_to_posts", p, posts)(_.id.?) // fk column name collision
  }
  val X = TableQuery[X]

  case class Category(id: Int, name: String)
  class Categories(tag: Tag) extends Table[Category](tag, "categories") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name", O.Length(254))
    def * = (id, name) <> (Category.tupled,Category.unapply)
    def idx = index("IDX_NAME",name)
  }
  val categories = TableQuery[Categories]

  class Posts(tag: Tag) extends Table[(Int, String, Option[Int])](tag, "POSTS") {
    def id = column[Int]("id")
    def title = column[String]("title")
    def category = column[Option[Int]]("category")
    def * = (id, title, category)
    def categoryFK = foreignKey("_", category, categories)(_.id.?)
  }
  val posts = TableQuery[Posts]

  // Clob disabled because it fails in postgres and mysql, see https://github.com/slick/slick/issues/637
  class TypeTest(tag: Tag) extends Table[(
    String,Boolean,Byte,Short,Int,Long,Float,Double,String,java.sql.Date,java.sql.Time,java.sql.Timestamp,java.util.UUID,java.sql.Blob//,java.sql.Clob
      ,Option[Int]
      ,(
      Option[Boolean],Option[Byte],Option[Short],Option[Int],Option[Long],Option[Float],Option[Double],Option[String],Option[java.sql.Date],Option[java.sql.Time],Option[java.sql.Timestamp],Option[java.util.UUID],Option[java.sql.Blob]//,Option[java.sql.Clob]
      )
    )](tag, "TYPE_TEST") {
    def `type` = column[String]("type") // <- test escaping of keywords
    def Boolean = column[Boolean]("Boolean",O.Default(true), O.Unique)
    def Byte = column[Byte]("Byte")
    def Short = column[Short]("Short")
    def Int = column[Int]("Int",O.Default(-5))
    def Long = column[Long]("Long",O.Default(5L))
    //def java_math_BigInteger = column[java.math.BigInteger]("java_math_BigInteger")
    def Float = column[Float]("Float",O.Default(9.999F))
    def Double = column[Double]("Double",O.Default(9.999))
    //def java_math_BigDecimal = column[java.math.BigDecimal]("java_math_BigDecimal")
    def String = column[String]("String",O.Default("someDefaultString"),O.Length(254))
    def java_sql_Date = column[java.sql.Date]("java_sql_Date")
    def java_sql_Time = column[java.sql.Time]("java_sql_Time")
    def java_sql_Timestamp = column[java.sql.Timestamp]("java_sql_Timestamp")
    def java_util_UUID = column[java.util.UUID]("java_util_UUID")
    def java_sql_Blob = column[java.sql.Blob]("java_sql_Blob")
    //def java_sql_Clob = column[java.sql.Clob]("java_sql_Clob")

    def None_Int = column[Option[Int]]("None_Int",O.Default(None))

    def Option_Boolean = column[Option[Boolean]]("Option_Boolean",O.Default(Some(true)))
    def Option_Byte = column[Option[Byte]]("Option_Byte")
    def Option_Short = column[Option[Short]]("Option_Short")
    def Option_Int = column[Option[Int]]("Option_Int",O.Default(Some(5)))
    def Option_Long = column[Option[Long]]("Option_Long",O.Default(Some(5L)))
    //def java_math_BigInteger = column[Option[java.math.BigInteger]]("java_math_BigInteger")
    def Option_Float = column[Option[Float]]("Option_Float",O.Default(Some(9.999F)))
    def Option_Double = column[Option[Double]]("Option_Double",O.Default(Some(9.999)))
    //def java_math_BigDecimal = column[Option[java.math.BigDecimal]]("java_math_BigDecimal")
    def Option_String = column[Option[String]]("Option_String",O.Default(Some("someDefaultString")),O.Length(254))
    def Option_java_sql_Date = column[Option[java.sql.Date]]("Option_java_sql_Date")
    def Option_java_sql_Time = column[Option[java.sql.Time]]("Option_java_sql_Time")
    def Option_java_sql_Timestamp = column[Option[java.sql.Timestamp]]("Option_java_sql_Timestamp")
    def Option_java_util_UUID = column[Option[java.util.UUID]]("Option_java_util_UUID")
    def Option_java_sql_Blob = column[Option[java.sql.Blob]]("Option_java_sql_Blob")
    def Option_java_sql_Option_Blob = column[Option[Option[java.sql.Blob]]]("Option_java_sql_Blob")
    //def Option_java_sql_Clob = column[Option[java.sql.Clob]]("Option_java_sql_Clob")
    def * = (
      `type`,
      Boolean,Byte,Short,Int,Long,Float,Double,String,java_sql_Date,java_sql_Time,java_sql_Timestamp,java_util_UUID,java_sql_Blob//,java_sql_Clob
      ,None_Int
      ,(
      Option_Boolean,Option_Byte,Option_Short,Option_Int,Option_Long,Option_Float,Option_Double,Option_String,Option_java_sql_Date,Option_java_sql_Time,Option_java_sql_Timestamp,Option_java_util_UUID,Option_java_sql_Blob//,Option_java_sql_Clob
      )
      )
    def pk = primaryKey("PK", (Int,Long))
  }
  val typeTest = TableQuery[TypeTest]

  // testing table larger 22 columns (code gen round trip does not preserve structure of the * projection or names of mapped to classes)
  case class Part(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int, i6: Int)
  case class Whole(id: Long, p1: Part, p2: Part, p3: Part, p4: Part, p5: Part, p6: Part)
  class Large(tag: Tag) extends Table[Whole](tag, "LARGE") {
    def id = column[Long]("id", O.PrimaryKey)
    def p1i1 = column[Int]("p1i1",O.Default(11))
    def p1i2 = column[Int]("p1i2",O.Default(12))
    def p1i3 = column[Int]("p1i3",O.Default(13))
    def p1i4 = column[Int]("p1i4",O.Default(14))
    def p1i5 = column[Int]("p1i5",O.Default(15))
    def p1i6 = column[Int]("p1i6",O.Default(16))
    def p2i1 = column[Int]("p2i1",O.Default(21))
    def p2i2 = column[Int]("p2i2",O.Default(22))
    def p2i3 = column[Int]("p2i3",O.Default(23))
    def p2i4 = column[Int]("p2i4",O.Default(24))
    def p2i5 = column[Int]("p2i5",O.Default(25))
    def p2i6 = column[Int]("p2i6",O.Default(26))
    def p3i1 = column[Int]("p3i1",O.Default(31))
    def p3i2 = column[Int]("p3i2",O.Default(32))
    def p3i3 = column[Int]("p3i3",O.Default(33))
    def p3i4 = column[Int]("p3i4",O.Default(34))
    def p3i5 = column[Int]("p3i5",O.Default(35))
    def p3i6 = column[Int]("p3i6",O.Default(36))
    def p4i1 = column[Int]("p4i1",O.Default(41))
    def p4i2 = column[Int]("p4i2",O.Default(42))
    def p4i3 = column[Int]("p4i3",O.Default(43))
    def p4i4 = column[Int]("p4i4",O.Default(44))
    def p4i5 = column[Int]("p4i5",O.Default(45))
    def p4i6 = column[Int]("p4i6",O.Default(46))
    def p5i1 = column[Int]("p5i1",O.Default(51))
    def p5i2 = column[Int]("p5i2",O.Default(52))
    def p5i3 = column[Int]("p5i3",O.Default(53))
    def p5i4 = column[Int]("p5i4",O.Default(54))
    def p5i5 = column[Int]("p5i5",O.Default(55))
    def p5i6 = column[Int]("p5i6",O.Default(56))
    def p6i1 = column[Int]("p6i1",O.Default(61))
    def p6i2 = column[Int]("p6i2",O.Default(62))
    def p6i3 = column[Int]("p6i3",O.Default(63))
    def p6i4 = column[Int]("p6i4",O.Default(64))
    def p6i5 = column[Int]("p6i5",O.Default(65))
    def p6i6 = column[Int]("p6i6",O.Default(66))
    def * = (
      id,
      (p1i1, p1i2, p1i3, p1i4, p1i5, p1i6),
      (p2i1, p2i2, p2i3, p2i4, p2i5, p2i6),
      (p3i1, p3i2, p3i3, p3i4, p3i5, p3i6),
      (p4i1, p4i2, p4i3, p4i4, p4i5, p4i6),
      (p5i1, p5i2, p5i3, p5i4, p5i5, p5i6),
      (p6i1, p6i2, p6i3, p6i4, p6i5, p6i6)
      ).shaped <> ({ case (id, p1, p2, p3, p4, p5, p6) =>
      // We could do this without .shaped but then we'd have to write a type annotation for the parameters
      Whole(id, Part.tupled.apply(p1), Part.tupled.apply(p2), Part.tupled.apply(p3), Part.tupled.apply(p4), Part.tupled.apply(p5), Part.tupled.apply(p6))
    }, { w: Whole =>
      def f(p: Part) = Part.unapply(p).get
      Some((w.id, f(w.p1), f(w.p2), f(w.p3), f(w.p4), f(w.p5), f(w.p6)))
    })
  }
  val large = TableQuery[Large]
}
