package scala.slick.direct

import scala.language.experimental.macros

import scala.reflect.macros.Context
import scala.slick.SlickException

import scala.reflect.runtime.{universe => ru}
import scala.reflect.ClassTag
import scala.slick.session.Session
import ru.TypeTag

object ImplicitQueryable extends BaseQueryableFactory{
  object implicitExecution{
    import language.implicitConversions
    implicit def implicitQueryableToSeq[T]( iq: ImplicitQueryable[T] ) : Seq[T] = iq.toSeq 
  }
  def apply[T]( q:BaseQueryable[T], backend:SlickBackend, session:Session ) = new ImplicitQueryable[T]( q, backend, session )
  def factory[S]( projection:ru.Expr[BaseQueryable[S]], backend:SlickBackend, session : Session ) : ImplicitQueryable[S] = {
    ImplicitQueryable( Queryable.factory[S]( projection ), backend, session )
  }
}
class ImplicitQueryableUtils[C <: Context]( context_ :C ) extends QueryableUtils[C]( context_ ) {
  import context.universe._
  import context._
  val backend = context.Expr[SlickBackend]( Select( prefix.tree, newTermName("backend") ) )
  val session = context.Expr[Session]( Select( prefix.tree, newTermName("session") ) )
  val queryable = Select( prefix.tree, newTermName("queryable") )
}

object ImplicitQueryableMacros{
  private def _scalar_helper[C <: Context, R]( c:C )( name:String ) = {
    val utils = new ImplicitQueryableUtils[c.type](c)
    import utils._
    c.universe.reify{
      backend.splice.result( new QueryableValue(
        select[Int](queryable, name).splice
      ), session.splice)
    }
  }
  private def _helper[C <: Context,S:c.WeakTypeTag,T]( c:C )( name:String, projection:c.Expr[_] ) : c.Expr[ImplicitQueryable[S]] = {
    val utils = new ImplicitQueryableUtils[c.type](c)
    import utils._
    c.universe.reify{
      ImplicitQueryable.factory(
          apply[Queryable[S]]( queryable, name, projection.tree ).splice
      , backend.splice, session.splice )
    }
  }
  def flatMap[T,S:c.WeakTypeTag]
    (c: scala.reflect.macros.Context)
    (projection: c.Expr[T => ImplicitQueryable[S]]): c.Expr[ImplicitQueryable[S]] = _helper[c.type,S,T]( c )( "flatMap", projection )
  def length[T]
      (c: scala.reflect.macros.Context)
      : c.Expr[Int] = _scalar_helper[c.type,Int]( c )( "length" )
  def map[T,S:c.WeakTypeTag]
    (c: scala.reflect.macros.Context)
    (projection: c.Expr[T => S]): c.Expr[ImplicitQueryable[S]] = _helper[c.type,S,T]( c )( "map", projection )
  def filter[T:c.WeakTypeTag]
    (c: scala.reflect.macros.Context)
    (projection: c.Expr[T => Boolean]): c.Expr[ImplicitQueryable[T]] = _helper[c.type,T,T]( c )( "filter", projection )
}

class ImplicitQueryable[T]( val queryable_ : BaseQueryable[T], val backend: SlickBackend, val session : Session ) extends BaseQueryable[T]( queryable_.expr_or_typetag ){
  import scala.collection._
  import scala.collection.generic._
  def toSeq : Seq[T] = backend.result( queryable, session )
  def map[S]( projection: T => S )                   : ImplicitQueryable[S] = macro ImplicitQueryableMacros.map[T,S]
  def flatMap[S]( projection: T => ImplicitQueryable[S] ) : ImplicitQueryable[S] = macro ImplicitQueryableMacros.flatMap[T,S]
  def filter    ( projection: T => Boolean )              : ImplicitQueryable[T] = macro ImplicitQueryableMacros.filter[T]
  def withFilter( projection: T => Boolean )              : ImplicitQueryable[T] = macro ImplicitQueryableMacros.filter[T]
  def length : Int = macro ImplicitQueryableMacros.length[T]
  def size   : Int = macro ImplicitQueryableMacros.length[T]
}
