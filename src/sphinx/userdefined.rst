User-Defined Features
=====================

This chapter describes how use custom data types and database functions
in the :ref:`Lifted Embedding <lifted-embedding>` API.

User-Defined Functions
----------------------

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

User-Defined Scalar Types
-------------------------

If you need a custom column type you can implement
:api:`ColumnType <scala.slick.driver.JdbcProfile@ColumnType>`. The most
common scenario is mapping an application-specific type to an already supported
type in the database. This can be done much simpler by using
:api:`MappedColumnType <scala.slick.driver.JdbcProfile@MappedColumnType>` which
takes care of all the boilerplate:

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

User-Defined Record Types
-------------------------

Out of the box, Slick supports Scala tuples (up to arity 22) and Slick's own
experimental :api:`scala.slick.collection.heterogenous.HList` implementation
(without any size limit) for record types, i.e. data structures containing a
fixed number of elements with individually defined types. These record types
can be nested and mixed arbitrarily.

If you need more flexibility, you can add implicit :api:`scala.slick.lifted.Shape`
definitions for other types like ``Pair`` in this example:

.. includecode:: code/LiftedEmbedding.scala#recordtypepair

``Shape`` implementations for record types extend
:api:`scala.slick.lifted.MappedScalaProductShape`. They are are generally very
simple but they require some boilerplate for all the types involved. A
``MappedScalaProductShape`` takes a sequence of Shapes for its elements and
provides the operations ``buildValue`` (for creating an instance of the record
type given its elements) and ``copy`` (for creating a copy of this ``Shape``
with new element Shapes):

.. includecode:: code/LiftedEmbedding.scala#recordtype1

The implicit method ``pairShape`` in this example provides a Shape for a
``Pair`` of two element types whenever Shapes for the inidividual element
types are available.

With these definitions in place, we can use the ``Pair`` record type in every
location in Slick where a tuple or ``HList`` would be acceptable:

.. includecode:: code/LiftedEmbedding.scala#recordtype2
