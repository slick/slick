package com.typesafe.slick.docs

import slick.jdbc.H2Profile.api._

object Cookbook {

  object MoreThan22FieldsPattern {
   
    //#imports22
    import slick.collection.heterogeneous.{HList, HCons, HNil}
    import slick.collection.heterogeneous.syntax._
    //#imports22
    
    //#example22
    case class Row(id: Int, name: String /* ... as many as you like */)

    class MyTable(tag: Tag) extends Table[Row](tag, "ROW") {
      def id   = column[Int]("ID", O.PrimaryKey)
      def name = column[String]("NAME")
      /* ... as many as you like */

      def * = (id :: name /* as many as you like... */ :: HNil).mapTo[Row]
    }
    //#example22
  }

}

