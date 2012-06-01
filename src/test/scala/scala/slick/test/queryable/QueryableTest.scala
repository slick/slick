package scala.slick.test.queryable

import scala.language.{reflectiveCalls,implicitConversions}

import org.junit.Test
import org.junit.Assert._
import scala.slick.ql._
import scala.slick.ql.ColumnOps.{Relational =>Op}
import scala.slick.ast._
import scala.slick.queryable._
import scala.slick.testutil._
import scala.slick.testutil.TestDB._

object QueryableTest extends DBTestObject(H2Mem)

class QueryableTest(val tdb: TestDB) extends DBTest {

  @table(name="COFFEES")
  trait CoffeesTable{
    @column(name="COF_SALES")
    def sales : Int
    @column(name="COF_NAME")
    def name : String
  }

  object backend extends SlickBackend(tdb.driver)

  object TestingTools{
    implicit def enableAssertQuery( q:Queryable[_] ) = new{
      def assertQuery( matcher : Node => Unit ) = {
        try{
          matcher( backend.toQuery( q ).node : @unchecked ) : @unchecked
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
    object ExpandedTable{
      def unapply( n:Node ) =
        n match {
          case Bind (
          sym1a,
          TableName(name),
          Pure( _ ))
          => Some(name)
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
  }

  @Test def test() {
    import TestingTools._
    val q : Queryable[CoffeesTable] = Queryable[CoffeesTable]

    //  q.dump
    println( backend.toSql(q) )

    /*
      // now checked later during translation
      try{
        Queryable[String]
        fail("expected exception about missing annotations")
      } catch{
        case e:Exception if e.getMessage.contains( "annotation" ) => success
        case e => fail
      }
    */

    // queryable
    q.assertQuery {
      case
        Bind (
        sym1a,
        TableName("COFFEES"),
        Pure(
        ProductNode(
        Select(Ref(sym1b), ColumnName("COF_NAME")),
        Select(Ref(sym1c), ColumnName("COF_SALES")) )))
        if sym1a == sym1b && sym1b == sym1c
      => ()
    }

    class MyQuerycollection{
      def findUserByName( name:String ) = q.filter( _.name == name )
      // FIXME:
      // def findUserByName2( name:String ) = Queryable[CoffeesTable].filter( _.name == name )
    }

    val qc = new MyQuerycollection
    qc.findUserByName("some value")
    //qc.findUserByName2("test")

    // simple map
    q.map( (_:CoffeesTable).sales + 5 )
      .assertQuery {
      case Bind(
      sym1a,
      ExpandedTable("COFFEES"),
      Pure(
      Op( "+", Select(Ref(sym1b), ColumnName("COF_SALES")), ConstColumn(5) )
      )
      )
        if sym1a == sym1b
      => ()
    }

    // map with string concatenation
    q.map( _.name + "." )
      .assertQuery {
      case Bind(
      sym1a,
      ExpandedTable("COFFEES"),
      Pure(
      Op( "concat", Select(Ref(sym1b), ColumnName("COF_NAME")), ConstColumn(".") )
      )
      )
        if sym1a == sym1b
      => ()
    }

    // filter with more complex condition
    q.filter( c => c.sales > 5 || "Chris" == c.name )
      .assertQuery {
      case Filter(
      sym1a,
      ExpandedTable("COFFEES"),
      Op( "||",
      Op( ">", Select(Ref(sym1b), ColumnName("COF_SALES")), ConstColumn(5) ),
      Op( "==", ConstColumn("Chris"), Select(Ref(sym1c), ColumnName("COF_NAME")) )
      )
      )
        if sym1a == sym1b && sym1b == sym1c
      => ()
    }

    // type annotations
    q.map[String]( (_:CoffeesTable).name : String )
      .assertQuery {
      case Bind(
      sym1a,
      ExpandedTable("COFFEES"),
      Pure(
      Select(Ref(sym1b), ColumnName("COF_NAME"))
      )
      )
        if sym1a == sym1b
      => ()
    }

    // chaining
    q.map( _.name ).filter(_ == "")
      .assertQuery {
      case Filter(
      sym1a,
      Bind(
      sym2a,
      ExpandedTable("COFFEES"),
      Pure(
      Select(Ref(sym2b), ColumnName("COF_NAME"))
      )
      ),
      Op( "==", Ref(sym1b), ConstColumn("") )
      )
        if sym1a == sym1b && sym2a == sym2b
      => ()
    }

    // referenced values are inlined as constants using reflection
    val o = 2 + 3
    q.filter( _.sales > o )
      .assertQuery {
      case Filter(
      sym1a,
      ExpandedTable("COFFEES"),
      Op( ">", Select(Ref(sym1b), ColumnName("COF_SALES")), ConstColumn(5) )
      )
        if sym1a == sym1b
      => ()
    }
    
    // BLOCKED by "[error] failed to typecheck against macro def return type: not found: value free$q8"
/*
      // nesting
      q.map(e1 => q.map(e2=>e1))
       .assertQuery {
          case Bind(
                 sym1a,
                 ExpandedTable("COFFEES"),
                 Pure(
                   Bind(
                     sym2a,
                     ExpandedTable("COFFEES"),
                     Pure(
                       Ref( sym1b )
                     )
          )))
          if sym1a == sym1b && sym1a != sym2a
          => ()
        }

      // query scope
      Queryable( q.filter( _.sales > 5 ) )
       .assertQuery {
          case Filter(
                  sym1a,
                  ExpandedTable("COFFEES"),
                  Op( ">", FieldRef(sym1b, ColumnName("COF_SALES")), ConstColumn(5) )
          )
          if sym1a == sym1b
          => ()
        }

      // comprehension with map
      (for( c <- q ) yield c.name).assertQuery{
        case Bind(
               sym1a,
               ExpandedTable("COFFEES"),
               Pure(
                 FieldRef( sym1b, ColumnName("COF_NAME") )
               )
        )
        if sym1a == sym1b
        => ()
      }

      // nesting with flatMap
      val pattern1 : Node => Unit =  {
        case Bind(
               sym1a,
               ExpandedTable("COFFEES"),
               Bind(
                 sym2a,
                 ExpandedTable("COFFEES"),
                 Pure(
                   FieldRef( sym2b, ColumnName("COF_NAME") )
        )))
        if sym1a != sym2a && sym2a == sym2b
        => ()
      }
               q.flatMap( o => q.map(i => i.name) ) .assertQuery{ pattern1 }
               (for( o <- q; i <- q ) yield i.name) .assertQuery{ pattern1 }
      Queryable(for( o <- q; i <- q ) yield i.name) .assertQuery{ pattern1 }

      // nesting with outer macro reference
      val pattern2 : Node => Unit =  {
        case Bind(
               sym1a,
               ExpandedTable("COFFEES"),
               Bind(
                 sym2a,
                 ExpandedTable("COFFEES"),
                 Pure(
                   FieldRef( sym1b, ColumnName("COF_NAME") )
        )))
        if sym1a != sym2a && sym1a == sym1b
        => ()
      }
               q.flatMap( o => q.map(i => o.name) ) .assertQuery{ pattern2 }
               (for( o <- q; i <- q ) yield o.name) .assertQuery{ pattern2 }
      Queryable(for( o <- q; i <- q ) yield o.name) .assertQuery{ pattern2 }

      // nesting with chaining / comprehension with cartesian product and if
      val pattern3 : Node => Unit =  {
        case Bind(
               sym1a,
               ExpandedTable("COFFEES"),
               Bind(
                 sym3a,
                 Filter(
                   sym2a,
                   ExpandedTable("COFFEES"),
                   Op( "==",
                       FieldRef( sym2b, ColumnName("COF_SALES") ),
                       FieldRef( sym1b, ColumnName("COF_SALES") )
                 )),
                 Pure(
                   FieldRef( sym3b, ColumnName("COF_NAME") )
        )))
        if sym1a != sym2a && sym2a != sym3a && sym1a == sym1b && sym3a == sym3b
        => ()
      }
      q.flatMap(o => q.filter( i => i.sales == o.sales ).map(i => i.name)) .assertQuery{ pattern3 }
               (for( o <- q; i <- q; if i.sales == o.sales ) yield i.name) .assertQuery{ pattern3 }
      Queryable(for( o <- q; i <- q; if i.sales == o.sales ) yield i.name) .assertQuery{ pattern3 }
*/
    /*
      //FAILS:
      (for( o <- Queryable[CoffeesTable];
                          i <- Queryable[CoffeesTable] ) yield (o.name,i.name))
    */
    /*  val d = 5.4
      val i = 5

      //FAILS: scala2scalaquery(scala.reflect.mirror.reify{5.4 + i}.tree )
      //FAILS: scala2scalaquery(scala.reflect.mirror.reify{d + 5}.tree )
      //FAILS: scala2scalaquery(scala.reflect.mirror.reify{i + 5.4}.tree )
      //FAILS: scala2scalaquery(scala.reflect.mirror.reify{5 + d}.tree )
    */
  }
}
