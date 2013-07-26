package com.typesafe.slick.examples.lifted
import scala.slick.driver.H2Driver.simple._

object HowSlickWorks extends App {
  object SomeTable extends Table[(Long,String)]("some_table") {
    def id = column[Long]("ID", O.PrimaryKey)
    def name = column[String]("NAME")
    def * = id ~ name
  }

  Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
    //#simpleQuery
    Query(SomeTable).filter( row => row.id === 5L ).map( row => row.name )
    //#simpleQuery
    //#helperFunction
    def idTest( id:Column[Long] ) = id === 5L
    Query(SomeTable).filter( row => idTest( row.id ) ).map( row => row.name )
    //#helperFunction
    val xs,ys = List(1,2,3,4)
    //#forComprehensions
    for( x <- xs; y <- ys; if x != y ) yield (x,y)
    //#forComprehensions
    //#desugaredComprehension
    xs.flatMap( x => ys.filter( y => x != y ).map( y => (x,y) ) )
    //#desugaredComprehension
    //#filter
    Query(SomeTable).filter( row => row.id === 5L )
    //#filter
    import language.reflectiveCalls
    val someList = List( new{ def id = 5L },  new{ def id = 6L } )
    //#collections
    someList.filter( element => element.id == 5L )
    //#collections
  }
}
