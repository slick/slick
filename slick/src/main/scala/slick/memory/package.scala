package slick

import slick.relational.ResultConverterDomainImpl

import scala.collection.mutable.ArrayBuffer

package object memory {

  type MemoryResultConverterDomain = ResultConverterDomainImpl[QueryInterpreter.ProductValue, ArrayBuffer[Any], Nothing]
}
