package slick.jdbc
import scala.language.experimental.macros

/**
  * Created by yjs on 2016/2/7.
  */

class GetResultReadsMacroImpl(val c: scala.reflect.macros.blackbox.Context) {

  import c.universe._

  def apply[Entity: c.WeakTypeTag] = {

    val entity = c.weakTypeOf[Entity]

    val entityConstructorSize = entity.typeSymbol.typeSignature.members
      .filter(_.isConstructor)
      .filter(_.asMethod.paramLists.head.nonEmpty)
      .size

    if (entityConstructorSize > 1)
      c.warning(
        c.enclosingPosition,
        s"GetResultMacro.reads only support primary constructor ,but ${show(entity)} has ${entityConstructorSize} constructor")

    val entityConstructor = entity.typeSymbol.typeSignature.members.filter(_.isConstructor).filter(_.asMethod.isPrimaryConstructor).head

    val entityParams = entityConstructor.asMethod.paramLists

    val positionedResultFunctionName = TermName("positionedResult")

    val positionedResultEntityConstructor = entityParams map (params ⇒ params.map(param ⇒ {
      def asOption = param.info <:< typeOf[Option[_]]

      if (asOption) {
        val tpe = param.info.typeArgs.tail.foldLeft(tq"${param.info.typeArgs.head}") { (l, r) ⇒
          tq"${l}[${r}]"
        }
        q"${positionedResultFunctionName}.<<?[$tpe]"
      }
      else {
        val tpe = tq"${param.info}"
        q"${positionedResultFunctionName}.<<[$tpe]"
      }
    }))
    val out =
      q"""
      slick.jdbc.GetResult((${positionedResultFunctionName} : slick.jdbc.PositionedResult)=> new $entity (...${positionedResultEntityConstructor}))
      """
    out
  }
}
