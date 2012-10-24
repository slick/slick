package scala.slick.direct
import scala.reflect.runtime.universe._

trait Mapper{
  def typeToTable( tpe: Type ) : String
  def fieldToColumn( sym:Symbol ) : String
  def isMapped( tpe:Type ) : Boolean
}

// FIXME: AnnotationMapper is implemented a bit hacky
object AnnotationMapper extends Mapper{
  import scala.annotation.StaticAnnotation
  final case class table(name:String = "") extends StaticAnnotation
  final case class column(name:String = "") extends StaticAnnotation
  def typeToTable( tpe : Type ) = {
    val sym = tpe.typeSymbol
    sym.annotations match {
      case Annotation( tpe, args, _ ) :: Nil // FIXME:<- don't match list, match any annotation
        //if tpe <:< classToType(classOf[table]) // genJVM bug
      =>
        args(0) match {
          case Literal(Constant(name:String)) => name
          case Select(_,term) if term.decoded == "<init>$default$1" => sym.name.decoded.toUpperCase // FIXME: make match more precise and don't hard code term name
          case _ => throw new Exception( "invalid argument to table annotation" )
        }
      case a => throw new Exception("Type argument passed to Queryable.apply needs database mapping annotations. None found on: " + tpe.toString )
    }
  }
  def fieldToColumn( sym:Symbol ) = (sym.annotations.collect{
    case Annotation( tpe, args, _ ) if tpe <:< typeOf[column]
      =>
        args(0) match {
          case Literal(Constant(name:String)) => name
          case Select(_,term) if term.decoded == "<init>$default$1" => sym.name.decoded.toUpperCase // FIXME: make match more precise and don't hard code term name
          case _ => throw new Exception( "invalid argument to column annotation" )
        }
  }).head
  def isMapped( tpe:Type ) = {
    val annotations = tpe.typeSymbol.annotations
    annotations.length > 0 && (annotations match {
      case Annotation(tpe,_,_) :: Nil
        if tpe <:< typeOf[table]
      => true
      case _ => false
    })
  }
}
