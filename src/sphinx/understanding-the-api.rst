Understanding the API
=====================

This chapter is mainly for people who are new to the *lifting* technique some Scala DSL's use. Slick uses it to implement it's lifted embedding API. Understanding how it works in principle makes Slick seem less magical, behave more expectedly and makes error messages easier to understand. So here is how Slick and lifting work.

The short story
-----------------

When you write a query using Slick's lifted embedding, like

.. includecode:: code/UnderstandingTheAPI.scala#simpleQuery

`row.id` is not of type `Long` as it may seem, but of type `Column[Long]` and `row.name` is of type `Column[String]`. Slick automatically "lifts" values of ordinary Scala types to the corresponding lifted types, `5L` is converted to a `Column[Long]` here. You can also do this by hand using `ConstColumn(5L)` and sometimes you actually have to because of limitations in Scala's type inference. When you write helper functions for Slick queries, you have to define them using `Column`-types instead of ordinary Scala types. 
For example you could write the above as

.. includecode:: code/UnderstandingTheAPI.scala#helperFunction

.. TODO: add note about Option types and Option propagation

.. TODO: add note about 5L === row.id not working but row.id === 5 working, see https://groups.google.com/forum/#!topic/scalaquery/-WOBG7Ozypo

The long story
-----------------

For-comprehensions
``````````````````````````

The first thing to know is the desugaring of for-comprehensions in Scala. The scala compiler translates  expressions like

.. includecode:: code/UnderstandingTheAPI.scala#forComprehensions

to a series of method calls. The above is translated to something like

.. includecode:: code/UnderstandingTheAPI.scala#desugaredComprehension

Slick implements the methods `filter`, `map`, `flatMap`, etc. for objects of type `Query`. This allows you to either use them with the for-comprehensions-syntax or call these methods on them directly. You can find more info on the desugaring in http://www.scala-lang.org/docu/files/ScalaReference.pdf .


Collections vs. lifted vs. direct embedding
````````````````````````````````````````````````````````````````````````````````````````````````````````

In Slick you can write queries like

.. includecode:: code/UnderstandingTheAPI.scala#filter

which look very similar to method calls on a Scala collections, e.g.

.. includecode:: code/UnderstandingTheAPI.scala#collections

With Scala collections, the argument to `.filter` is an anonymous function that is translated to Java Bytecode by the Scala compiler and executed by the JVM for every element. A `.filter` call in Slick looks (almost) identical, but Slick has to analyze the argument and translate it to SQL so that the database can run it for every row instead of the JVM. This can be implemented either using lifting, which the lifted embedding implements or using macros, which the direct embedding implements. Lifting is more explicit and more flexible but also requires some more knowledge about its implementation to really understand Slick's API especially when it comes to types. Also lifting in Scala has a few syntactic limitations, which among other things require the lifted embedding to use the `===` operator instead of `==`. These slight differences between Scala collections and Slick's lifted embedding can be surprising and need to be remembered. The direct embedding API has different limitations and may be easier to understand. It uses ordinary Scala types, you can use ordinary `==`, etc. The current implementation of direct embedding however is experimental, limited and needs more exploration. Lifted embedding and direct embedding are currently not interoperable. We are working on improving the direct embedding and making the two interoperable. Until then we strongly recommend the lifted embedding for serious projects.

Lifted embedding
````````````````````````````````````````````````````````````````````````````````````````````````````````

The lifted embedding API uses a technique we call *lifting* to analyze the anonymous functions given to its higher order methods like `.filter`. Instead of applying a function to each actual database row, Slick applies it once to a prototype of a row (an instance of a subclass of `Table`) of type `Column[T]` where `T` is the type of the row, e.g. a `Tuple` or a `case class`. As return value Slick expects a tree representation of the expression in the function's body in return, which needs to be of type `Column[S]` where type `S` depends on the method you called. In case of `.filter` Slick expects an anonymous function, which returns a `Column[Boolean]` encapsulating an expression which evaluates to a `Boolean` when applied to a particular database row. You can find an example in section *The short story* above.

.. TODO: maybe remove the following paragraph in favor of the lifted embedding docs page

A Slick-type `Column[T]` supports a subset of the methods supported on it's ordinary Scala equivalent `T`. Slick supports a few additional methods like isNull, etc. Similarly, a Slick query is of type `Query[T]` where `T` is the type of the row, which is equivalent to the element type of a Scala collection. `Query[T]` supports a subset of the methods supported by Scala collections and a few additional ones for outer joins, etc. These methods return again objects of type `Query[_]`. Queries can be executed meaning that a Query of T is iterated over using the `.foreach` method or turned into a Scala collection of T using `.run`.

.. TODO: link methods to API docs, change inline code style from italic to <pre>, link "the short story" internally
