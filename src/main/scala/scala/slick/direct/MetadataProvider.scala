package scala.slick.direct
import scala.reflect.runtime.universe._

trait Mapper{
  def typeToTable( tpe: Type ) : String
  def fieldToColumn( sym:Symbol ) : String
  def isMapped( tpe:Type ) : Boolean
}

object AnnotationMapper extends Mapper{
  import scala.annotation.StaticAnnotation
  final case class table(name:String) extends StaticAnnotation
  final case class column(name:String) extends StaticAnnotation
  def typeToTable( tpe : Type ) = {
    tpe.typeSymbol.getAnnotations match {
      case AnnotationInfo(tpe,tree,_) :: Nil // FIXME:<- don't match list, match any annotation
        //if tpe <:< classToType(classOf[table]) // genJVM bug
      =>
      {
        val name = tree(0).toString
        name.slice( 1,name.length-1 ) // FIXME: <- why needed?
      }
      case a => throw new Exception("Type argument passed to Queryable.apply needs database mapping annotations. None found on: " + tpe.toString )
    }
  }
  def fieldToColumn( sym:Symbol ) = sym.getAnnotations.collect{
    case x@AnnotationInfo(tpe,tree,_)
        if tpe <:< typeOf[column]
      => { // FIXME: is this the right way to do it?
        val name = tree(0).toString
          name.slice( 1,name.length-1 ) // FIXME: <- why needed?
      }
  }.head
  def isMapped( tpe:Type ) = {
    val annotations = tpe.typeSymbol.getAnnotations
    annotations.length > 0 && (annotations match {
      case AnnotationInfo(tpe,_,_) :: Nil
        if tpe <:< typeOf[table]
      => true
      case _ => false
    })
  }
}
