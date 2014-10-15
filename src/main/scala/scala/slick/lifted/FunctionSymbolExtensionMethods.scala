package scala.slick.lifted

import scala.language.implicitConversions
import scala.slick.ast.{TypedType, Node, FunctionSymbol}

/** Utility methods for internal use in the lifted embedding */
final class FunctionSymbolExtensionMethods(val fs: FunctionSymbol) /*extends AnyVal*/ {
  /** Create a Column with a typed Apply of this Symbol */
  def column[T : TypedType](ch: Node*) = Rep.forNode[T](fs.typed(implicitly[TypedType[T]], ch: _*))
}

object FunctionSymbolExtensionMethods {
  implicit def functionSymbolExtensionMethods(fs: FunctionSymbol) = new FunctionSymbolExtensionMethods(fs)
}
