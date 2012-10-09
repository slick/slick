package scala.slick.lifted

import scala.language.implicitConversions
import scala.slick.ast.{Node, FunctionSymbol}

/** Utility methods for internal use in the lifted embedding */
final class FunctionSymbolExtensionMethods(val fs: FunctionSymbol) /*extends AnyVal*/ {
  /** Create a Column with a typed Apply of this Symbol */
  def column[T : TypeMapper](ch: Node*) = Column.forNode[T](fs.typed(implicitly[TypeMapper[T]], ch: _*))
}

object FunctionSymbolExtensionMethods {
  implicit def functionSymbolExtensionMethods(fs: FunctionSymbol) = new FunctionSymbolExtensionMethods(fs)
}
