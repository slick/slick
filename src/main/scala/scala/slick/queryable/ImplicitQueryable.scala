package scala.slick.queryable

import scala.language.experimental.macros

import scala.reflect.makro.Context
import scala.slick.SlickException

import scala.reflect.runtime.{universe => ru}
import scala.reflect.ClassTag
import scala.slick.session.Session
import ru.TypeTag

object ImplicitQueryable{
  def apply[T]( q:Queryable[T], backend:SlickBackend )( implicit session:Session ) = new ImplicitQueryable[T]( q, backend, session )
/*  def apply[T:ru.TypeTag:ClassTag] = new ImplicitQueryable[T](Right( (implicitly[ru.TypeTag[T]],implicitly[ClassTag[T]]) ))
  def factory[S]( projection:ru.Expr[ImplicitQueryable[S]] ) : ImplicitQueryable[S] = {
    new ImplicitQueryable[S]( Left(projection) )
  }*/
}

/*
class BoundImplicitQueryable[T]( backend : SlickBackend ){
  class ImplicitQueryable[T](
    val expr_or_typetag : Either[ reflect.basis.Expr[_], reflect.basis.TypeTag[_] ]
  ) extends slick.ImplicitQueryable( expr_or_typetag ){
    def toList = backend.toList( this )
  }
}
*/


object ImplicitQueryableMacros{
  private def _scalar_helper[C <: Context]( c:C )( name:String ) = {
    import c.universe._
    
    //val element_type = implicitly[c.TypeTag[S]].tpe
    val queryable = c.Expr[QueryableValue[Int]]( Select( Select( c.prefix.tree, newTermName("queryable") ), newTermName(name) ) )
    val backend = c.Expr[SlickBackend]( Select( c.prefix.tree, newTermName("backend") ) )
    val session = c.Expr[Session]( Select( c.prefix.tree, newTermName("session") ) )
    
    c.reify{
      implicit val _session = session.splice
      backend.splice.result( queryable.splice )
    }
  }
  def length[T]
      (c: scala.reflect.makro.Context)
      (): c.Expr[Int] = _scalar_helper[c.type]( c )( "length" )
}



class ImplicitQueryable[T]( val queryable : Queryable[T], val backend: SlickBackend, val session : Session ) {
  /*def map[S]    ( projection: T => S )                    : ImplicitQueryable[S] = ImplicitQueryable( queryable.map       (projection), backend )
  def flatMap[S]( projection: T => ImplicitQueryable[S] ) : ImplicitQueryable[S] = ImplicitQueryable( queryable.flatMap   (projection), backend )
  def filter    ( projection: T => Boolean )              : ImplicitQueryable[T] = ImplicitQueryable( queryable.filter    (projection), backend )
  def withFilter( projection: T => Boolean )              : ImplicitQueryable[T] = ImplicitQueryable( queryable.withFilter(projection), backend )*/
  def length() : Int = macro ImplicitQueryableMacros.length[T]
}
