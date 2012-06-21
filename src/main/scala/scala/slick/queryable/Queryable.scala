package scala.slick.queryable

import scala.language.experimental.macros

import scala.reflect.makro.Context
import scala.slick.SLICKException

import scala.annotation.StaticAnnotation

import scala.reflect.runtime.{universe => ru}
import scala.reflect.ClassTag


final case class table(name:String) extends StaticAnnotation
final case class column(name:String) extends StaticAnnotation

object Queryable{
  def apply[T](q:Queryable[T]) = new Queryable[T](q.expr_or_typetag) // TODO: make this a macro
  def apply[T:ru.TypeTag:ClassTag] = new Queryable[T](Right( (implicitly[ru.TypeTag[T]],implicitly[ClassTag[T]]) ))
  def factory[S]( projection:ru.Expr[Queryable[S]] ) : Queryable[S] = {
    new Queryable[S]( Left(projection) )
  }
}

class UnsupportedMethodException(msg : String = "" ) extends SLICKException(msg)

case class Utils[C <: Context]( c:C ) {
  import c.universe._
  import c.{Tree=>_}
  object removeDoubleReify extends Transformer {
    def apply( tree:Tree ) = transform(tree)
    override def transform(tree: Tree): Tree = {
      super.transform {
        tree match {
          case  //Apply( // needed to account for ApplyToImplicitArgs
            Apply(TypeApply(Select(_this, termname), _), reified::Nil )
            //,_)
            if termname.toString == "factory" => c.unreifyTree(reified)
          case //Apply(
            Apply(Select(_this, termname), reified::Nil )
            //,_)
            if termname.toString == "factory" => c.unreifyTree(reified)
          case _ => tree
        }
      }
    }
  }
}

object QueryableMacros{
  private def _helper[C <: Context,S:c.TypeTag]( c:C )( name:String, projection:c.Expr[_] ) = {
    import c.universe._
    //val element_type = implicitly[c.TypeTag[S]].tpe
    val reifiedExpression = c.Expr[ru.Expr[Queryable[S]]](
      c.reifyTree( c.runtimeUniverse, EmptyTree, c.typeCheck(
        Utils[c.type](c).removeDoubleReify(
          Apply(Select(c.prefix.tree, newTermName( "_"+name+"_placeholder" )), List( projection.tree ))
        ).asInstanceOf[Tree]
      )))
    c.reify{ Queryable.factory[S]( reifiedExpression.splice )}
  }

  def map[T:c.TypeTag, S:c.TypeTag]
  (c: scala.reflect.makro.Context)
  (projection: c.Expr[T => S]): c.Expr[scala.slick.queryable.Queryable[S]] = _helper[c.type,S]( c )( "map", projection )
  def flatMap[T:c.TypeTag, S:c.TypeTag]
  (c: scala.reflect.makro.Context)
  (projection: c.Expr[T => Queryable[S]]): c.Expr[scala.slick.queryable.Queryable[S]] = _helper[c.type,S]( c )( "flatMap", projection )
  def filter[T:c.TypeTag]
  (c: scala.reflect.makro.Context)
  (projection: c.Expr[T => Boolean]): c.Expr[scala.slick.queryable.Queryable[T]] = _helper[c.type,T]( c )( "filter", projection )
}

/*
class BoundQueryable[T]( backend : SlickBackend ){
  class Queryable[T](
    val expr_or_typetag : Either[ reflect.basis.Expr[_], reflect.basis.TypeTag[_] ]
  ) extends slick.Queryable( expr_or_typetag ){
    def toList = backend.toList( this )
  }
}
*/

class Queryable[T](
                    val expr_or_typetag : Either[ ru.Expr[_], (ru.TypeTag[_],ClassTag[_]) ]
                    ){
  def _map_placeholder[S]( projection: T => S ) : Queryable[S] = ???
  def map[S]( projection: T => S ) : Queryable[S] = macro QueryableMacros.map[T,S]

  def _flatMap_placeholder[S]( projection: T => Queryable[S] ) : Queryable[S] = ???
  def flatMap[S]( projection: T => Queryable[S] ) : Queryable[S] = macro QueryableMacros.flatMap[T,S]

  def _filter_placeholder( projection: T => Boolean ) : Queryable[T] = ???
  def filter( projection: T => Boolean ) : Queryable[T] = macro QueryableMacros.filter[T]
  def withFilter( projection: T => Boolean ) : Queryable[T] = macro QueryableMacros.filter[T]
}
