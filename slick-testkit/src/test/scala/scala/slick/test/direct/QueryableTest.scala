package scala.slick.test.direct
import scala.slick.direct.order._

import scala.language.{reflectiveCalls,implicitConversions}
import org.junit.Test
import org.junit.Assert._
import scala.slick.lifted._
import scala.slick.ast
import scala.slick.direct._
import scala.slick.direct.AnnotationMapper._
import scala.slick.testutil._
import slick.jdbc.StaticQuery.interpolation
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag
import scala.slick.SlickException
import com.typesafe.slick.testkit.util.JdbcTestDB

object QueryableTest extends DBTestObject(TestDBs.H2Mem)

class Foo[T]( val q : Queryable[T] )

@table(name="COFFEES")
case class Coffee(
  @column(name="COF_NAME")
  name : String
  ,@column // <- assumes "SALES" automatically
  sales : Int
  ,@column
  flavor : Option[String]
)
object Singleton{
  val q = Queryable[Coffee]
  val q1 = q.map( _.sales + 5 )
  object Singleton{
    val q = Queryable[Coffee]
    val q1 = q.map( _.sales + 5 )
    object Singleton{
      val q = Queryable[Coffee]
      val q1 = q.map( _.sales + 5 )
    }
  }
}

class QueryableTest(val tdb: JdbcTestDB) extends DBTest {
  import tdb.driver.backend.Database.dynamicSession

  object backend extends SlickBackend(tdb.driver,AnnotationMapper)

  object TestingTools{
    def enableAssertQuery[T:TypeTag:ClassTag]( q:Queryable[T] ) = new{
      def assertQuery( matcher : ast.Node => Unit ) = {
        //backend.dump(q)
        println( backend.toSql(q,dynamicSession) )
        println( backend.result(q,dynamicSession) )
        try{
          matcher( backend.toQuery( q )._2.node : @unchecked ) : @unchecked
          print(".")
        } catch {
          case e:MatchError => {
            println("F")
            println("")
            backend.dump(q)
            assert(false,"did not match")
          }
        }
      }
    }
    object TableName{
      def unapply( t:ast.TableNode ) = {
        val name = t.tableName
        Some(name)
      }
    }
    object ColumnName{
      def unapply( t:ast.Symbol ) = t match {
        case ast.FieldSymbol( name ) =>
          /*case RawNamedColumn( name, _, _ ) =>*/
          Some(name)
      }
    }
    def fail(msg:String = ""){
      println("F")
      throw new Exception(msg)
    }
    def fail : Unit = fail()
    def success{ print(".") }
    def assertEqualMultiSet[T]( lhs:scala.collection.Traversable[T], rhs:scala.collection.Traversable[T] ) = assertEquals( rhs.groupBy(x=>x), lhs.groupBy(x=>x) )
    def assertMatch[T:TypeTag:ClassTag]( queryable:Queryable[T], expected: Traversable[T] ) = assertEqualMultiSet( backend.result(queryable,dynamicSession), expected)
    def assertNotEqualMultiSet[T]( lhs:scala.collection.Traversable[T], rhs:scala.collection.Traversable[T] ) = assertEquals( lhs.groupBy(x=>x), rhs.groupBy(x=>x) )
    def assertNoMatch[T:TypeTag:ClassTag]( queryable:Queryable[T], expected: Traversable[T] ) = try{
      assertEqualMultiSet( backend.result(queryable,dynamicSession), expected)
    } catch {
      case e:AssertionError => 
      case e:Throwable => throw e
    }
    def assertMatchOrdered[T:TypeTag:ClassTag]( queryable:Queryable[T], expected: Traversable[T] ) = assertEquals( expected, backend.result(queryable,dynamicSession) )
  }

  object SingletonInClass{
    val qoo = Queryable[Coffee]
    val q1 = qoo.map( _.sales + 5 )
  }
  def initialStringOptionOrdering = implicitly[Ordering[Option[String]]]
  
  @Test def test() {
    implicit var stringOptionOrdering : scala.math.Ordering[Option[String]] = initialStringOptionOrdering

    import TestingTools._
    
    val coffees_data = Vector(
      ("Colombian",          1, None),
      ("French_Roast",       2, None),
      ("Espresso",           3, Some("Honey")),
      ("Espresso",           3, Some("Vanilla")),
      ("Espresso",           4, None),
      ("Colombian_Decaf",    1, None),
      ("Colombian_Decaf",    3, Some("White Chocolate")),
      ("French_Roast_Decaf", 5, None)
    )
    
    db withDynSession {
      // create test table
      sqlu"create table COFFEES(COF_NAME varchar(255), SALES int, FLAVOR varchar(255) NULL)".execute
      (for {
        (name, sales, flavor) <- coffees_data
      } yield sqlu"insert into COFFEES values ($name, $sales, $flavor)".first).sum

      // FIXME: reflective typecheck failed:  backend.result(Queryable[Coffee].map(x=>x))
      
      // setup query and equivalent inMemory data structure
      val inMem = coffees_data.map{ case (name, sales,flavor) => Coffee(name, sales,flavor) }
      val query : Queryable[Coffee] = Queryable[Coffee]

      val iquery = ImplicitQueryable( query, backend, dynamicSession )
      assertEquals( iquery.length, inMem.length )
      
      // iquery.filter( _.sales > 10.0 ).map( _.name ) // currently crashed compiler
      
      assertMatch(
        query.map( c=>c ),
        iquery.map( c=>c ).toSeq
      )
      
      
      ({
        import ImplicitQueryable.implicitExecution._
        val i1 = iquery.map( c=>c )
        
        assertMatch(
          query.map( c=>c ),
          iquery.map( c=>c ) // found: Seq[scala.slick.test.direct.Coffee], required: scala.slick.direct.ImplicitQueryable[scala.slick.test.direct.Coffee]
        )
        

      })
      
    }
  }
}
