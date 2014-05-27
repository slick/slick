:tocdepth: 2

.. index:: introduction

Introduction
############

What is Slick?
--------------

Slick is `Typesafe <http://www.typesafe.com>`_'s modern database query and access library for Scala. It allows you
to work with stored data almost as if you were using Scala collections while
at the same time giving you full control over when a database access happens
and which data is transferred. You can also use SQL directly.

.. includecode:: code/GettingStartedOverview.scala#what-is-slick-micro-example

When using Scala instead of raw SQL for your queries you benefit from compile-time safety
and compositionality. Slick can generate queries for different back-end databases including
your own, using its extensible query compiler.

Get started learning Slick in minutes using the `Hello Slick <http://typesafe.com/activator/template/hello-slick>`_ template in 
`Typesafe Activator <http://typesafe.com/activator>`_.


Features
--------

Scala
_____
* Queries, Table & Column Mappings, and types are plain Scala

.. includecode:: code/GettingStartedOverview.scala#quick-schema

* Data access APIs similar to Scala collections

.. includecode:: code/GettingStartedOverview.scala#features-scala-collections

Type Safe
_________
* Let your IDE help you write your code
* Find problems at compile-time instead of at runtime

.. includecode:: code/GettingStartedOverview.scala#features-type-safe

Composable
__________
* Queries are functions that can be composed and reused

.. includecode:: code/GettingStartedOverview.scala#features-composable


Compatibility
-------------

Slick requires Scala 2.10. (For Scala 2.9 please use ScalaQuery_, the predecessor of Slick).

.. _supported-dbs:

Supported database systems
--------------------------

* DB2 (via :doc:`slick-extensions <extensions>`)
* Derby/JavaDB
* H2
* HSQLDB/HyperSQL
* Microsoft Access
* Microsoft SQL Server (via :doc:`slick-extensions <extensions>`)
* MySQL
* Oracle (via :doc:`slick-extensions <extensions>`)
* PostgreSQL
* SQLite

Other SQL databases can be accessed right away with a reduced feature set.
Writing a fully featured plugin for your own SQL-based backend can be achieved
with a reasonable amount of work. Support for other backends (like NoSQL) is
under development but not yet available.

License
-------
Slick is released under a BSD-Style free and open source software :slick:`license <LICENSE.txt>`.
See the chapter on the commercial :doc:`Slick Extensions <extensions>` add-on
package for details on licensing the Slick drivers for the big commercial
database systems.

Query APIs
----------

The *Lifted Embedding* is the standard API for type-safe queries and updates
in Slick. Please see :doc:`gettingstarted` for an introduction. Most of this
user manual focuses on the *Lifted Embedding*.

For writing your own SQL statements you can use the :doc:`Plain SQL<sql>` API.

The experimental :doc:`Direct Embedding <direct-embedding>` is available as an
alternative to the *Lifted Embedding*.

.. _lifted-embedding:

Lifted Embedding
----------------

The name *Lifted Embedding* refers to the fact that you are not working with
standard Scala types (as in the :doc:`direct embedding <direct-embedding>`)
but with types that are *lifted* into a :api:`scala.slick.lifted.Rep` type
constructor. This becomes clear when you compare the types of a simple
Scala collections example

.. includecode:: code/LiftedEmbedding.scala#plaintypes

... with the types of similar code using the lifted embedding:

.. includecode:: code/LiftedEmbedding.scala#reptypes

All plain types are lifted into ``Rep``. The same is true for the table row
type ``Coffees`` which is a subtype of ``Rep[(String, Double)]``.
Even the literal ``8.0`` is automatically lifted to a ``Rep[Double]`` by an
implicit conversion because that is what the ``>`` operator on
``Rep[Double]`` expects for the right-hand side. This lifting is necessary
because the lifted types allow us to generate a syntax tree that captures
the query computations. Getting plain Scala functions and values would not
give us enough information for translating those computations to SQL.
