package scala.slick

final case class table(name:String) extends StaticAnnotation
final case class column(name:String) extends StaticAnnotation
import scala.reflect.makro.Context

object Queryable{
  def apply[T](q:Queryable[T]) = new Queryable[T](q.query) // TODO: make this a macro
  def apply[T:reflect.ConcreteTypeTag] = new Queryable[T](scala2scalaquery.classToQuery[T])
  def factory[S]( projection:scala.reflect.mirror.Expr[Queryable[S]] ) : Queryable[S] = {
    new Queryable[S](scala2scalaquery( projection.tree ))
  }
}

class UnsupportedMethodException(msg : String = "" ) extends Exception(msg)

case class Utils[C <: Context]( c:C ) {
  import c.mirror._
  import c.{Tree=>_}
  object removeDoubleReify extends c.mirror.Transformer {
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
  private def _helper[C <: Context,S:c.TypeTag]( c:C )( name:String, projection:c.mirror.Expr[_] ) = {
    import c.mirror._
    val element_type = implicitly[TypeTag[S]].tpe
    // 
/*
        val tree = Apply(Select(c.prefix.tree, newTermName( "_"+name+"_placeholder" )), List( projection.tree ))
        println("------------------")
        println(showRaw(tree))
        println("------------------")
*/

    val reifiedExpression = Expr[reflect.mirror.Expr[Queryable[S]]](
    c.reifyTree( c.reflectMirrorPrefix, c.typeCheck(
      Utils[c.type](c).removeDoubleReify(
        Apply(Select(c.prefix.tree, newTermName( "_"+name+"_placeholder" )), List( projection.tree ))
       ).asInstanceOf[Tree]
      )))
    c.reify{ Queryable.factory[S]( reifiedExpression.eval )}
  }

  def map[T:c.TypeTag, S:c.TypeTag]
               (c: scala.reflect.makro.Context)
               (projection: c.mirror.Expr[T => S]): c.mirror.Expr[scala.slick.Queryable[S]] = _helper[c.type,S]( c )( "map", projection )
  def flatMap[T:c.TypeTag, S:c.TypeTag]
               (c: scala.reflect.makro.Context)
               (projection: c.mirror.Expr[T => Queryable[S]]): c.mirror.Expr[scala.slick.Queryable[S]] = _helper[c.type,S]( c )( "flatMap", projection )
  def filter[T:c.TypeTag]
               (c: scala.reflect.makro.Context)
               (projection: c.mirror.Expr[T => Boolean]): c.mirror.Expr[scala.slick.Queryable[T]] = _helper[c.type,T]( c )( "filter", projection )
}

class Queryable[T]( protected[slick] val query:scala2scalaquery.Query ){
  def dump = scala2scalaquery.dump(query)
  def _map_placeholder[S]( projection: T => S ) : Queryable[S] = ???
  def map[S]( projection: T => S ) : Queryable[S] = macro QueryableMacros.map[T,S]
  
  def _flatMap_placeholder[S]( projection: T => Queryable[S] ) : Queryable[S] = ???
  def flatMap[S]( projection: T => Queryable[S] ) : Queryable[S] = macro QueryableMacros.flatMap[T,S]

  def _filter_placeholder( projection: T => Boolean ) : Queryable[T] = ???
  def filter( projection: T => Boolean ) : Queryable[T] = macro QueryableMacros.filter[T]
}

