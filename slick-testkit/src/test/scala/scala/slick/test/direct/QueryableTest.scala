package scala.slick.test.direct

import scala.language.{reflectiveCalls,implicitConversions}
import org.junit.Test
import org.junit.Assert._
import scala.slick.lifted._
import scala.slick.ast.Library.{SqlOperator =>Op,_}
import scala.slick.ast.{Library => Ops}
import scala.slick.ast._
import scala.slick.direct._
import scala.slick.direct.AnnotationMapper._
import scala.slick.testutil._
import slick.jdbc.StaticQuery.interpolation
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag
import com.typesafe.slick.testkit.util.TestDB


object QueryableTest extends DBTestObject(TestDBs.H2Mem)

class Foo[T]( val q : Queryable[T] )

@table(name="COFFEES")
case class Coffee(
  @column(name="COF_NAME")
  name : String,
  @column // <- assumes "SALES" automatically
  sales : Int
)

class QueryableTest(val tdb: TestDB) extends DBTest {
  import tdb.driver.backend.Database.threadLocalSession

  object backend extends SlickBackend(tdb.driver,AnnotationMapper)

  object TestingTools{
    def enableAssertQuery[T:TypeTag:ClassTag]( q:Queryable[T] ) = new{
      def assertQuery( matcher : Node => Unit ) = {
        //backend.dump(q)
        println( backend.toSql(q,threadLocalSession) )
        println( backend.result(q,threadLocalSession) )
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
      def unapply( t:TableNode ) = {
        val name = t.tableName
        Some(name)
      }
    }
    object ColumnName{
      def unapply( t:Symbol ) = t match {
        case FieldSymbol( name ) =>
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
    def isEqualMultiSet[T]( lhs:scala.collection.Traversable[T], rhs:scala.collection.Traversable[T] ) = lhs.groupBy(x=>x) == rhs.groupBy(x=>x)
    def resultsMatch[T:TypeTag:ClassTag]( queryable:Queryable[T], expected: Traversable[T] ) = isEqualMultiSet( backend.result(queryable,threadLocalSession), expected)
  }

  @Test def test() {
    import TestingTools._
    
    val coffees_data = Vector(
      ("Colombian",          1),
      ("French_Roast",       2),
      ("Espresso",           3),
      ("Colombian_Decaf",    4),
      ("French_Roast_Decaf", 5)
    )
    
    db withSession {
      // create test table
      sqlu"create table COFFEES(COF_NAME varchar(255), SALES int)".execute
      (for {
        (name, sales) <- coffees_data
      } yield sqlu"insert into COFFEES values ($name, $sales)".first).sum

      // FIXME: reflective typecheck failed:  backend.result(Queryable[Coffee].map(x=>x))
      
      // setup query and equivalent inMemory data structure
      val inMem = coffees_data.map{ case (name, sales) => Coffee(name, sales) }
      val query : Queryable[Coffee] = Queryable[Coffee]

      // test framework sanity checks
      assert( ! resultsMatch(query, inMem ++ inMem) )
      assert( ! resultsMatch(query, List()) )

      // fetch whole table
      assert( resultsMatch( query, inMem ) )

      // FIXME: make this test less artificial
      class MyQuerycollection{
        def findUserByName( name:String ) = query.filter( _.name == name )
      }  
      val qc = new MyQuerycollection
      qc.findUserByName("some value")
  
      // simple map
      assert( resultsMatch(
        query.map( (_:Coffee).sales + 5 ),
        inMem.map( (_:Coffee).sales + 5 )
      ))
      
      // left-hand-side coming from attribute
      val foo = new Foo(query)
      assert( resultsMatch(
        foo.q.map( (_:Coffee).sales + 5 ),
        inMem.map( (_:Coffee).sales + 5 )
      ))
  
      // map with string concatenation
      assert( resultsMatch(
        query.map( _.name + "." ),
        inMem.map( _.name + "." )
      ))
  
      // filter with more complex condition
      assert( resultsMatch(
        query.filter( c => c.sales > 5 || "Chris" == c.name ),
        inMem.filter( c => c.sales > 5 || "Chris" == c.name )
      ))
  
      // type annotations FIXME canBuildFrom
      assert( resultsMatch(
        query.map[String]( (_:Coffee).name : String ),
        inMem.map        ( (_:Coffee).name : String )
      ))

      // chaining
      assert( resultsMatch(
        query.map( _.name ).filter(_ == ""),
        inMem.map( _.name ).filter(_ == "")
      ))
  
      // referenced values are inlined as constants using reflection
      val o = 2 + 3
      assert( resultsMatch(
        query.filter( _.sales > o ),
        inMem.filter( _.sales > o )
      ))

      // nesting (not supported yet: query.map(e1 => query.map(e2=>e1))) 
      assert( resultsMatch(
        query.flatMap(e1 => query.map(e2=>e1)),
        inMem.flatMap(e1 => inMem.map(e2=>e1))
      ))
  
      // query scope
      {
        val inMemResult = inMem.filter( _.sales > 5 )
        List(
          query.filter( _.sales > 5 ),
          Queryable( query.filter( _.sales > 5 ) ),
          Queryable{
            val foo = query
            val bar = foo.filter( _.sales > 5 )
            bar  
          }
        ).foreach{
          query_ => assert( resultsMatch( query_, inMemResult ) )
        }
      }

      // comprehension with map
      assert( resultsMatch(
        for( c <- query ) yield c.name,
        for( c <- inMem ) yield c.name
      ))
  
      // nesting with flatMap
      {
        val inMemResult = for( o <- inMem; i <- inMem ) yield i.name
        List(
                   query.flatMap( o => query.map(i => i.name) ),
                    for( o <- query; i <- query ) yield i.name ,
          Queryable(for( o <- query; i <- query ) yield i.name)
        ).foreach{
          query_ => assert( resultsMatch( query_, inMemResult ) )
        }
      }

      assert( resultsMatch(
        query.flatMap(e1 => query.map(e2=>e1).map(e2=>e1)),
        inMem.flatMap(e1 => inMem.map(e2=>e1).map(e2=>e1))
      )) 

      // nesting with outer macro reference
      {
        val inMemResult = for( o <- inMem; i <- inMem ) yield o.name
        List(
                   query.flatMap( o => query.map(i => o.name) ),
                   for( o <- query; i <- query ) yield o.name ,
          Queryable(for( o <- query; i <- query ) yield o.name)
        ).foreach{
          query_ => assert( resultsMatch( query_, inMemResult ) )
        }
      }
  
      // nesting with chaining / comprehension with cartesian product and if
      {
        val inMemResult = for( o <- inMem; i <- inMem; if i.sales == o.sales ) yield i.name
        List(
          query.flatMap(o => query.filter( i => i.sales == o.sales ).map(i => i.name)),
                    for( o <- query; i <- query; if i.sales == o.sales ) yield i.name ,
          Queryable(for( o <- query; i <- query; if i.sales == o.sales ) yield i.name)
        ).foreach{
          query_ => assert( resultsMatch( query_, inMemResult ) )
        }
      }

      // tuples
      assert( resultsMatch(
        query.map( c=> (c.name,c.sales) ),
        inMem.map( c=> (c.name,c.sales) )
      ))
      
      // nested structures (here tuples and case classes)
      assert( resultsMatch(
        query.map( c=> (c.name,c.sales,c) ),
        inMem.map( c=> (c.name,c.sales,c) )
      ))
      // length
      assertEquals( backend.result(query.length,threadLocalSession), inMem.length )

      val iquery = ImplicitQueryable( query, backend, threadLocalSession )
      assertEquals( iquery.length, inMem.length )
      
      // iquery.filter( _.sales > 10.0 ).map( _.name ) // currently crashed compiler
      
      assert( resultsMatch(
        query.map( c=>c ),
        iquery.map( c=>c ).toSeq
      ))
      
      ({
        import ImplicitQueryable.implicitExecution._
        assert( resultsMatch(
          query.map( c=>c ),
          iquery.map( c=>c )
        ))
      })
   
      assert( resultsMatch(
           for( o <-  query; i <-  query; if i.sales == o.sales  ) yield i.name,
          (for( o <- iquery; i <- iquery; if i.sales == o.sales ) yield i.name).toSeq
      ))
      
      assert(resultsMatch(
        for( v1<-query;v2<-query; if !(v1.name == v2.name)) yield (v1.name,v2.name)
        ,for( v1<-inMem;v2<-inMem; if !(v1.name == v2.name)) yield (v1.name,v2.name)
      ))
      
      assert(resultsMatch(
        for( v1<-query;v2<-query; if v1.name != v2.name) yield (v1.name,v2.name)
        ,for( v1<-inMem;v2<-inMem; if v1.name != v2.name) yield (v1.name,v2.name)
      ))
    }
  }
}
