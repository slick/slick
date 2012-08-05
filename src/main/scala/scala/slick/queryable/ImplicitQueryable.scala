package scala.slick.queryable

import scala.language.experimental.macros

import scala.reflect.makro.Context
import scala.slick.SlickException

import scala.reflect.runtime.{universe => ru}
import scala.reflect.ClassTag
import scala.slick.session.Session
import ru.TypeTag

object ImplicitQueryable{
  object implicitExecution{
    import language.implicitConversions
    implicit def implicitQueryableToSeq[T]( iq: ImplicitQueryable[T] ) : Seq[T] = iq.toSeq 
  }
  def apply[T]( q:Queryable[T], backend:SlickBackend, session:Session ) = new ImplicitQueryable[T]( q, backend, session )
}


object ImplicitQueryableMacros{
  private def _scalar_helper[C <: Context, R]( c:C )( name:String ) = {
    import c.universe._

    val queryable = c.Expr[QueryableValue[R]]( Select( Select( c.prefix.tree, newTermName("queryable") ), newTermName(name) ) )
    val backend = c.Expr[SlickBackend]( Select( c.prefix.tree, newTermName("backend") ) )
    val session = c.Expr[Session]( Select( c.prefix.tree, newTermName("session") ) )

    reify{
      backend.splice.result( queryable.splice, session.splice )
    }
  }
  private def _helper[C <: Context,S,T]( c:C )( name:String, projection:c.Expr[T => S] ) : c.Expr[ImplicitQueryable[T]] = {
    import c.universe._
    
    val queryable = c.Expr[Queryable[T]]( Apply(Select( Select( c.prefix.tree, newTermName("queryable") ), newTermName(name) ), List(projection.tree)) )
    val backend = c.Expr[SlickBackend]( Select( c.prefix.tree, newTermName("backend") ) )
    val session = c.Expr[Session]( Select( c.prefix.tree, newTermName("session") ) )
  
    reify{
      ImplicitQueryable( queryable.splice, backend.splice, session.splice )
    }
  }
  def length[T]
      (c: scala.reflect.makro.Context)
      : c.Expr[Int] = _scalar_helper[c.type,Int]( c )( "length" )
  def map[T,S]
    (c: scala.reflect.makro.Context)
    (projection: c.Expr[T => S]): c.Expr[ImplicitQueryable[T]] = _helper[c.type,S,T]( c )( "map", projection )
}

class ImplicitQueryable[T]( val queryable : Queryable[T], val backend: SlickBackend, val session : Session ){
  import scala.collection._
  import scala.collection.generic._
  def toSeq : Seq[T] = backend.result( queryable, session )
  def map[S]( projection: T => S )                   : ImplicitQueryable[S] = macro ImplicitQueryableMacros.map[T,S]
/*  def flatMap[S:TypeTag:ClassTag]( projection: T => ImplicitQueryable[S] ) : ImplicitQueryable[S] = ImplicitQueryable( queryable.flatMap[S](projection), backend )
  def filter    ( projection: T => Boolean )              : ImplicitQueryable[T] = ImplicitQueryable( queryable.filter    (projection), backend )
  def withFilter( projection: T => Boolean )              : ImplicitQueryable[T] = ImplicitQueryable( queryable.withFilter(projection), backend )*/
  def length : Int = macro ImplicitQueryableMacros.length[T]
  def size   : Int = macro ImplicitQueryableMacros.length[T]
}
