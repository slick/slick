package slick.ast

sealed abstract class LockStrength(val sqlName: String)

object LockStrength {
  case object ForUpdate extends LockStrength("for update")
  case object ForShare extends LockStrength("for share")
}
