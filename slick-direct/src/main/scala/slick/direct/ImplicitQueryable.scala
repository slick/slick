package slick.direct

import scala.language.experimental.macros

import scala.reflect.macros.Context
import slick.SlickException

import scala.reflect.runtime.{universe => ru}
import scala.reflect.ClassTag
import ru.TypeTag


@deprecated("The Direct Embedding will be removed. Use the Lifted Embedding instead.", "3.0")
object ImplicitQueryable extends BaseQueryableFactory{
  object implicitExecution{
    import language.implicitConversions
    implicit def implicitQueryableToSeq[T]( iq: ImplicitQueryable[T] ) : Seq[T] = iq.toSeq 
  }
  def apply[T]( q:BaseQueryable[T], backend:SlickBackend, database:SlickBackend#Database ) = new ImplicitQueryable[T]( q, backend, database )
  def factory[S]( projection:ru.Expr[BaseQueryable[S]], backend:SlickBackend, database : SlickBackend#Database ) : ImplicitQueryable[S] = {
    ImplicitQueryable( Queryable.factory[S]( projection ), backend, database )
  }
}
@deprecated("The Direct Embedding will be removed. Use the Lifted Embedding instead.", "3.0")
class ImplicitQueryableUtils[C <: Context]( context_ :C ) extends QueryableUtils[C]( context_ ) {
  import context.universe._
  import context._
  val backend = context.Expr[SlickBackend]( Select( prefix.tree, newTermName("backend") ) )
  val database = context.Expr[SlickBackend#Database]( Select( prefix.tree, newTermName("database") ) )
  val queryable = Select( prefix.tree, newTermName("queryable") )
}

@deprecated("The Direct Embedding will be removed. Use the Lifted Embedding instead.", "3.0")
object ImplicitQueryableMacros{
  private def _scalar_helper[C <: Context, R]( c:C )( name:String ) = {
    val utils = new ImplicitQueryableUtils[c.type](c)
    import utils._
    c.universe.reify{
      ImplicitQueryableMacros._run(backend.splice, database.splice, new QueryableValue(select[Int](queryable, name).splice))
    }
  }
  private def _helper[C <: Context,S:c.WeakTypeTag,T]( c:C )( name:String, projection:c.Expr[_] ) : c.Expr[ImplicitQueryable[S]] = {
    val utils = new ImplicitQueryableUtils[c.type](c)
    import utils._
    c.universe.reify{
      ImplicitQueryable.factory(
          apply[Queryable[S]]( queryable, name, projection.tree ).splice
      , backend.splice, database.splice )
    }
  }
  def _run[T](backend: SlickBackend, db: SlickBackend#Database, qv: QueryableValue[T]): T =
    Blocking.run(db, backend.result(qv))
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

@deprecated("The Direct Embedding will be removed. Use the Lifted Embedding instead.", "3.0")
class ImplicitQueryable[T]( val queryable_ : BaseQueryable[T], val backend: SlickBackend, val database : SlickBackend#Database ) extends BaseQueryable[T]( queryable_.expr_or_typetag ){
  import scala.collection._
  import scala.collection.generic._
  def toSeq : Seq[T] = Blocking.run(database, backend.result(queryable))
  def map[S]( projection: T => S )                   : ImplicitQueryable[S] = macro ImplicitQueryableMacros.map[T,S]
  def flatMap[S]( projection: T => ImplicitQueryable[S] ) : ImplicitQueryable[S] = macro ImplicitQueryableMacros.flatMap[T,S]
  def filter    ( projection: T => Boolean )              : ImplicitQueryable[T] = macro ImplicitQueryableMacros.filter[T]
  def withFilter( projection: T => Boolean )              : ImplicitQueryable[T] = macro ImplicitQueryableMacros.filter[T]
  def length : Int = macro ImplicitQueryableMacros.length[T]
  def size   : Int = macro ImplicitQueryableMacros.length[T]
}
