package scala.slick.typeProviders

import language.experimental.macros

object TypeProvider {
	type Db(url: String, configurationFileName: String = "configuration") = macro Macros.DbImpl
}