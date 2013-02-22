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
  import tdb.driver.backend.Database.threadLocalSession

  object backend extends SlickBackend(tdb.driver,AnnotationMapper)

  object TestingTools{
    def enableAssertQuery[T:TypeTag:ClassTag]( q:Queryable[T] ) = new{
      def assertQuery( matcher : ast.Node => Unit ) = {
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
    def assertMatch[T:TypeTag:ClassTag]( queryable:Queryable[T], expected: Traversable[T] ) = assertEqualMultiSet( backend.result(queryable,threadLocalSession), expected)
    def assertNotEqualMultiSet[T]( lhs:scala.collection.Traversable[T], rhs:scala.collection.Traversable[T] ) = assertEquals( lhs.groupBy(x=>x), rhs.groupBy(x=>x) )
    def assertNoMatch[T:TypeTag:ClassTag]( queryable:Queryable[T], expected: Traversable[T] ) = try{
      assertEqualMultiSet( backend.result(queryable,threadLocalSession), expected)
    } catch {
      case e:AssertionError => 
      case e:Throwable => throw e
    }
    def assertMatchOrdered[T:TypeTag:ClassTag]( queryable:Queryable[T], expected: Traversable[T] ) = assertEquals( expected, backend.result(queryable,threadLocalSession) )
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
    
    db withSession {
      // create test table
      sqlu"create table COFFEES(COF_NAME varchar(255), SALES int, FLAVOR varchar(255) NULL)".execute
      (for {
        (name, sales, flavor) <- coffees_data
      } yield sqlu"insert into COFFEES values ($name, $sales, $flavor)".first).sum

      // FIXME: reflective typecheck failed:  backend.result(Queryable[Coffee].map(x=>x))
      
      // setup query and equivalent inMemory data structure
      val inMem = coffees_data.map{ case (name, sales,flavor) => Coffee(name, sales,flavor) }
      val query : Queryable[Coffee] = Queryable[Coffee]

      // test framework sanity checks
      assertNoMatch(query, inMem ++ inMem)
      assertNoMatch(query, List())

      // fetch whole table
      assertMatch( query, inMem )

      // FIXME: make this test less artificial
      class MyQuerycollection{
        def findUserByName( name:String ) = query.filter( _.name == name )
      }  
      val qc = new MyQuerycollection
      qc.findUserByName("some value")
      
      // test singleton object
      assertMatch(
        Singleton.q1,
        inMem.map( _.sales + 5 )
      )
      
      // test singleton in package
      assertMatch(
        Singleton.q1,
        inMem.map( _.sales + 5 )
      )
      
      // test singleton in singleton in package
      assertMatch(
        Singleton.Singleton.q1,
        inMem.map( _.sales + 5 )
      )

      // test singleton in singleton in singleton in package
      assertMatch(
        Singleton.Singleton.Singleton.q1,
        inMem.map( _.sales + 5 )
      )

      // test singleton in class (not supported (yet?))
      try{
        assertMatch(
          SingletonInClass.q1,
          inMem.map( _.sales + 5 )
        )
        fail()
      }catch{
        case _:SlickException => 
      }

      // simple map
      assertMatch(
        query.map( (_:Coffee).sales + 5 ),
        inMem.map( (_:Coffee).sales + 5 )
      )
      
      // left-hand-side coming from attribute
      val foo = new Foo(query)
      assertMatch(
        foo.q.map( (_:Coffee).sales + 5 ),
        inMem.map( (_:Coffee).sales + 5 )
      )
  
      // map with string concatenation
      assertMatch(
        query.map( _.name + "." ),
        inMem.map( _.name + "." )
      )
  
      // filter with more complex condition
      assertMatch(
        query.filter( c => c.sales > 5 || "Chris" == c.name ),
        inMem.filter( c => c.sales > 5 || "Chris" == c.name )
      )
  
      // type annotations FIXME canBuildFrom
      assertMatch(
        query.map[String]( (_:Coffee).name : String ),
        inMem.map        ( (_:Coffee).name : String )
      )

      // chaining
      assertMatch(
        query.map( _.name ).filter(_ == ""),
        inMem.map( _.name ).filter(_ == "")
      )
  
      // referenced values are inlined as constants using reflection
      val o = 2 + 3
      assertMatch(
        query.filter( _.sales > o ),
        inMem.filter( _.sales > o )
      )

      // nesting (not supported yet: query.map(e1 => query.map(e2=>e1))) 
      assertMatch(
        query.flatMap(e1 => query.map(e2=>e1)),
        inMem.flatMap(e1 => inMem.map(e2=>e1))
      )
  
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
          query_ => assertMatch( query_, inMemResult )
        }
      }

      // comprehension with map
      assertMatch(
        for( c <- query ) yield c.name,
        for( c <- inMem ) yield c.name
      )
  
      // nesting with flatMap
      {
        val inMemResult = for( o <- inMem; i <- inMem ) yield i.name
        List(
                   query.flatMap( o => query.map(i => i.name) ),
                    for( o <- query; i <- query ) yield i.name ,
          Queryable(for( o <- query; i <- query ) yield i.name)
        ).foreach{
          query_ => assertMatch( query_, inMemResult )
        }
      }

      assertMatch(
        query.flatMap(e1 => query.map(e2=>e1).map(e2=>e1)),
        inMem.flatMap(e1 => inMem.map(e2=>e1).map(e2=>e1))
      ) 

      // nesting with outer macro reference
      {
        val inMemResult = for( o <- inMem; i <- inMem ) yield o.name
        List(
                   query.flatMap( o => query.map(i => o.name) ),
                   for( o <- query; i <- query ) yield o.name ,
          Queryable(for( o <- query; i <- query ) yield o.name)
        ).foreach{
          query_ => assertMatch( query_, inMemResult )
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
          query_ => assertMatch( query_, inMemResult )
        }
      }

      // tuples
      assertMatch(
        query.map( c=> (c.name,c.sales) ),
        inMem.map( c=> (c.name,c.sales) )
      )
      
      // nested structures (here tuples and case classes)
      assertMatch(
        query.map( c=> (c.name,c.sales,c) ),
        inMem.map( c=> (c.name,c.sales,c) )
      )
      // length
      assertEquals( backend.result(query.length,threadLocalSession), inMem.length )

      val iquery = ImplicitQueryable( query, backend, threadLocalSession )
      assertEquals( iquery.length, inMem.length )
      
      // iquery.filter( _.sales > 10.0 ).map( _.name ) // currently crashed compiler
      
      assertMatch(
        query.map( c=>c ),
        iquery.map( c=>c ).toSeq
      )
      
      ({
        import ImplicitQueryable.implicitExecution._
        assertMatch(
          query.map( c=>c ),
          iquery.map( c=>c )
        )
      })
   
      assertMatch(
           for( o <-  query; i <-  query; if i.sales == o.sales  ) yield i.name,
          (for( o <- iquery; i <- iquery; if i.sales == o.sales ) yield i.name).toSeq
      )
      
      assertMatch(
        for( v1<-query;v2<-query; if !(v1.name == v2.name)) yield (v1.name,v2.name)
        ,for( v1<-inMem;v2<-inMem; if !(v1.name == v2.name)) yield (v1.name,v2.name)
      )
      
      assertMatch(
        for( v1<-query;v2<-query; if v1.name != v2.name) yield (v1.name,v2.name)
        ,for( v1<-inMem;v2<-inMem; if v1.name != v2.name) yield (v1.name,v2.name)
      )
      
      assertMatch(
        query.take(2)
        ,inMem.take(2)
      )
      
      assertMatch(
        query.drop(2)
        ,inMem.drop(2)
      )
      
      assertMatchOrdered(
        query.sortBy(_.name)
        ,inMem.sortBy(_.name)
      )
      
      assertMatchOrdered(
        query.sortBy(c=>(c.name,c.flavor))
        ,inMem.sortBy(c=>(c.name,c.flavor))
      )
      
      assertMatchOrdered(
        query.sortBy(c => reversed(c.name))
        ,inMem.sortBy(c => reversed(c.name))
      )      
      
      assertMatchOrdered(
        query.sortBy(c => (c.name,reversed(c.sales)))
        ,inMem.sortBy(c => (c.name,reversed(c.sales)))
      )
      
      def nullOrdering( x:Int, y:Int ) = new scala.math.Ordering[Option[String]]{
        def compare(a:Option[String],b:Option[String]) = {
          if( a == None && b == None ) 0
          else if( a == None ) x * -1
          else if( b == None ) x * 1
          else y * (a.get compare b.get)
        }
      }

      stringOptionOrdering = nullOrdering(-1,1)
      assertMatchOrdered(
        query.sortBy(c=>(nonesLast(c.flavor),c.name))
        ,inMem.sortBy(c=>(c.flavor,c.name))
      )

      stringOptionOrdering = nullOrdering(1,1)
      assertMatchOrdered(
        query.sortBy(c=>(nonesFirst(c.flavor),c.name))
        ,inMem.sortBy(c=>(c.flavor,c.name))
      )

      stringOptionOrdering = nullOrdering(-1,-1)
      assertMatchOrdered(
        query.sortBy(c=>(nonesLast(reversed(c.flavor)),c.name))
        ,inMem.sortBy(c=>(c.flavor,c.name))
      )

      stringOptionOrdering = nullOrdering(1,-1)
      assertMatchOrdered(
        query.sortBy(c=>(nonesFirst(reversed(c.flavor)),c.name))
        ,inMem.sortBy(c=>(c.flavor,c.name))
      )
      
      stringOptionOrdering = initialStringOptionOrdering
      assertMatchOrdered(
        query.sortBy( c => (
          c.name
          ,reversed(c.sales)
          ,reversed(c.flavor)
        ))
        ,inMem.sortBy( c => (
          c.name
          ,reversed(c.sales)
          ,reversed(c.flavor)
        ))
      )
    }
  }
  @Test def sortingTest(){
//    def assertEquals[T]( a:T, b:T ) = assert( a == b)
    val cA0 = Coffee("A",0,None)
    val cA1 = Coffee("A",1,None)
    val cB0 = Coffee("B",0,None)
    val cB1 = Coffee("B",1,None)
    val cB1_ = Coffee("B",1,Some("X"))
    val coffees = List( cA1, cB0, cA0, cB1, cB1_ )
    assertEquals(
      List(cA1,cA0,cB0,cB1,cB1_),
      coffees.sortBy(_.name)
    )
    assertEquals(
      List(cA1,cA0,cB0,cB1,cB1_),
      coffees.sortBy(_.name)
    )
    assertEquals(
      List(cB0,cB1,cB1_,cA1,cA0),
      coffees.sortBy(c=>reversed(c.name))
    )
    assertEquals(
      List(cA1,cA0,cB1,cB1_,cB0),
      coffees.sortBy(c => (c.name,reversed(c.sales)))
    )
    //assertEquals( coffees.sortBy(_.name,asc), by(_.sales,desc) ), List(cA1,cA0,cB1,cB0) )
    assertEquals(
      List(cA1,cA0,cB1_,cB1,cB0),
      coffees.sortBy( c => (
        c.name
        ,reversed(c.sales)
        ,reversed(c.flavor)
      ))
    )
  }
}
