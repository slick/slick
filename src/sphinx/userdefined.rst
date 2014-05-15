User-Defined Features
=====================

This chapter describes how to use custom data types and database functions
in the :ref:`Lifted Embedding <lifted-embedding>` API.

Scalar Database functions
--------------------------

If your database system supports a scalar function that is not available as
a method in Slick you can define it as a
:api:`scala.slick.lifted.SimpleFunction`. There are predefined methods for
creating unary, binary and ternary functions with fixed parameter and return
types.

.. includecode:: code/LiftedEmbedding.scala#simplefunction1

If you need more flexibility regarding the types (e.g. for varargs,
polymorphic functions, or to support Option and non-Option types in a single
function), you can use ``SimpleFunction.apply`` to get an untyped instance and
write your own wrapper function with the proper type-checking:

.. includecode:: code/LiftedEmbedding.scala#simplefunction2

:api:`scala.slick.lifted.SimpleBinaryOperator` and
:api:`scala.slick.lifted.SimpleLiteral` work in a similar way. For even more
flexibility (e.g. function-like expressions with unusual syntax), you can
use :api:`scala.slick.lifted.SimpleExpression`.

Other Database functions and stored procedures
----------------------------------------------

For database functions that return complete tables or stored procedures please use :doc:`sql`.
Stored procedures that return multiple result sets are currently not supported.

Using custom scalar types in queries
---------------------------------------

If you need a custom column type you can implement
:api:`ColumnType <scala.slick.driver.JdbcProfile@ColumnType[T]:JdbcDriver.ColumnType[T]>`. The most
common scenario is mapping an application-specific type to an already supported
type in the database. This can be done much simpler by using
:api:`MappedColumnType <scala.slick.driver.JdbcProfile@MappedColumnType:JdbcDriver.MappedJdbcType.type>` which
takes care of all the boilerplate. It comes with the usual import from the driver. Do not import it from the :api:`JdbcDriver <scala.slick.driver.JdbcDriver$>` singleton object.

.. includecode:: code/LiftedEmbedding.scala#mappedtype1

You can also subclass
:api:`MappedJdbcType <scala.slick.driver.JdbcProfile@MappedJdbcType>`
for a bit more flexibility.

If you have a wrapper class (which can optionally be a case class and/or value
class) for an underlying value of some supported type, you can make it extend
:api:`scala.slick.lifted.MappedTo` to get a macro-generated implicit
``ColumnType`` for free. Such wrapper classes are commonly used for type-safe
table-specific primary key types:

.. includecode:: code/LiftedEmbedding.scala#mappedtype2

.. _record-types:

Using custom structured types in queries
----------------------------------------

Structured types are data structures containing a statically known
number of components with individually declared types.  Out of the box,
Slick supports Scala tuples (up to arity 22) and Slick's own
experimental :api:`scala.slick.collection.heterogenous.HList` implementation.

In order to use custom structured data types
(case classes, custom hlists, tuple-like types, ...)
in queries you need to tell Slick how to map them between queries
and results. You can do that using a :api:`scala.slick.lifted.Shape`
extending :api:`scala.slick.lifted.MappedScalaProductShape`.

Mapped structured types can be arbitrarily combined.

Nestable types (e.g. custom tuple types or hlists)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can add support for custom nestable types using an appropriate
:api:`scala.slick.lifted.Shape`.

Here is an example for a type ``Pair``:

.. includecode:: code/LiftedEmbedding.scala#recordtype1

The implicit method ``pairShape`` in this example provides a Shape for a
``Pair`` of two element types whenever Shapes for the inidividual element
types are available.

With these definitions in place, we can use the ``Pair`` record type in every
location in Slick where a tuple or ``HList`` would be acceptable:

.. includecode:: code/LiftedEmbedding.scala#recordtype2

Custom case classes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

I order to map a custom case class using a :api:`scala.slick.lifted.Shape`,
you need to write
two variants. One case class with ``Column``  typed members for use in
queries and one with ordinary Scala types for use in results.

Here is an example using a ``CaseClassShape`` snippet to easily create
a Shape for the mapping:

.. includecode:: code/LiftedEmbedding.scala#case-class-shape

Combining mapped types
^^^^^^^^^^^^^^^^^^^^^^^^^
In the following example we are combining a mapped case class and the mapped
``Pair`` type in another mapped case class.

.. includecode:: code/LiftedEmbedding.scala#combining-shapes
