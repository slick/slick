/*
 * Copyright (C) 2018-2022 Lightbend Inc. <https://www.lightbend.com>
 */

package slick.compat

package object collection {
  private[slick] type Factory[-A, +C] = scala.collection.Factory[A, C]
  private[slick] val Factory = scala.collection.Factory

  private[slick] type LazyList[+T] = scala.collection.immutable.LazyList[T]
  private[slick] val LazyList = scala.collection.immutable.LazyList

  object JavaConverters
      extends scala.collection.convert.AsJavaExtensions
      with scala.collection.convert.AsScalaExtensions

}
