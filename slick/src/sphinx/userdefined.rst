.. index:: user-defined

User-Defined Features
=====================

This chapter describes how to use custom data types and database functions
with Slick's Scala API.

.. index::
   triple: user-defined; scalar; function

.. _scalar-db-functions:

Scalar Database Functions
-------------------------

If your database system supports a scalar function that is not available as
a method in Slick you can define it as a
:api:`slick.lifted.SimpleFunction`. There are predefined methods for
creating unary, binary and ternary functions with fixed parameter and return
types.

.. includecode:: code/LiftedEmbedding.scala#simplefunction1

If you need more flexibility regarding the types (e.g. for varargs,
polymorphic functions, or to support Option and non-Option types in a single
function), you can use ``SimpleFunction.apply`` to get an untyped instance and
write your own wrapper function with the proper type-checking:

.. includecode:: code/LiftedEmbedding.scala#simplefunction2

:api:`slick.lifted.SimpleBinaryOperator` and
:api:`slick.lifted.SimpleLiteral` work in a similar way. For even more
flexibility (e.g. function-like expressions with unusual syntax), you can
use :api:`slick.lifted.SimpleExpression`.

.. includecode:: code/LiftedEmbedding.scala#simpleliteral

Other Database Functions And Stored Procedures
----------------------------------------------

For database functions that return complete tables or stored procedures please use :doc:`sql`.
Stored procedures that return multiple result sets are currently not supported.

.. index:: MappedColumnType, MappedJdbcType
.. index::
   triple: user-defined; scalar; type
   pair: mapped; type

Using Custom Scalar Types in Queries
------------------------------------

If you need a custom column type you can implement
:api:`ColumnType <slick.driver.JdbcProfile@ColumnType[T]:JdbcDriver.ColumnType[T]>`. The most
common scenario is mapping an application-specific type to an already supported type in the database.
This can be done much simpler by using
:api:`MappedColumnType <slick.driver.JdbcProfile@MappedColumnType:JdbcDriver.MappedJdbcType.type>`
which takes care of all the boilerplate. It comes with the usual import from the driver.

.. includecode:: code/LiftedEmbedding.scala#mappedtype1

You can also subclass
:api:`MappedJdbcType <slick.driver.JdbcProfile@MappedJdbcType>`
for a bit more flexibility.

.. index:: MappedTo

If you have a wrapper class (which can optionally be a case class and/or value
class) for an underlying value of some supported type, you can make it extend
:api:`slick.lifted.MappedTo` to get a macro-generated implicit
``ColumnType`` for free. Such wrapper classes are commonly used for type-safe
table-specific primary key types:

.. includecode:: code/LiftedEmbedding.scala#mappedtype2

.. index:: Shape
.. index::
   triple: user-defined; record; type
.. _record-types:

Using Custom Record Types in Queries
------------------------------------

Record types are data structures containing a statically known
number of components with individually declared types.  Out of the box,
Slick supports Scala tuples (up to arity 22) and Slick's own
:api:`slick.collection.heterogeneous.HList` implementation. Record
types can be nested and mixed arbitrarily.

In order to use custom record types (case classes, custom HLists, tuple-like
types, etc.) in queries you need to tell Slick how to map them between queries
and results. You can do that using a :api:`slick.lifted.Shape`
extending :api:`slick.lifted.MappedScalaProductShape`.

Polymorphic Types (e.g. Custom Tuple Types or HLists)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The distinguishing feature of a *polymorphic* record type is that it abstracts
over its element types, so you can use the same record type for both, lifted
and plain element types. You can add support for custom polymorphic record
types using an appropriate implicit :api:`slick.lifted.Shape`.

Here is an example for a type ``Pair``:

.. includecode:: code/LiftedEmbedding.scala#recordtype1

The implicit method ``pairShape`` in this example provides a Shape for a
``Pair`` of two element types whenever Shapes for the individual element
types are available.

With these definitions in place, we can use the ``Pair`` record type in every
location in Slick where a tuple or ``HList`` would be acceptable:

.. includecode:: code/LiftedEmbedding.scala#recordtype2

Monomorphic Case Classes
^^^^^^^^^^^^^^^^^^^^^^^^

Custom *case classes* are frequently used as monomorphic record types (i.e.
record types where the element types are fixed). In order to use them in Slick,
you need to define the case class for a record of plain values (as usual) plus
an additional case class for a matching record of lifted values.

In order to provide a :api:`slick.lifted.Shape` for a custom case class,
you can use :api:`slick.lifted.CaseClassShape`:

.. includecode:: code/LiftedEmbedding.scala#case-class-shape

Note that this mechanism can be used as an alternative to client-side mappings
with the `<>` operator. It requires a bit more boilerplate but allows you to use
the same field names in both, plain and lifted records.

Combining Mapped Types
^^^^^^^^^^^^^^^^^^^^^^

In the following example we are combining a mapped case class and the mapped
``Pair`` type in another mapped case class.

.. includecode:: code/LiftedEmbedding.scala#combining-shapes
