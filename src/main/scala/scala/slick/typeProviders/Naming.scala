package scala.slick.typeProviders

object Naming {
  def tableSQLToModule(tableSQL: String): String = {
    tableSQL.toLowerCase.capitalize
  }

  def moduleToCaseClass(module: String): String = {
    if (module.endsWith("s"))
      module.dropRight(1)
    else
      "C" + module
  }

  def columnSQLToField(columnSQL: String): String = {
    columnSQL.toLowerCase.foldLeft[(String, Boolean)](("", false))((prev, c) => {
      val (str, flag) = prev
      (c, flag) match {
        case ('_', _) => (str, true)
        case (_, true) => (str + Character.toUpperCase(c), false)
        case _ => (str + c, false)
      }
    })._1
  }
}