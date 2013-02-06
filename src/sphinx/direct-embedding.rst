Direct Embedding
================

The direct embedding is a new, experimental front-end for writing queries. The API may change without deprecation during experimental status.
Unlike the (stable) lifted embedding, the direct embedding uses macros instead of operator overloading and implicit conversions for its implementation.
For a user the difference in the code is small, but queries using the direct embedding work with ordinary Scala types,
which can make error messages easier to understand. 

The following descriptions are anolog to the description of the :doc:`lifted embedding <gettingstarted>`.

Dependencies
------------

The direct embedding requires access to the Scala compiler at runtime for
typechecking. Slick only declares an *optional* dependency on scala-compiler
in order to avoid bringing it (along with all transitive dependencies) into
your application if you don't need it. You must add it explicitly to your
own project's ``build.sbt`` file if you want to use the direct embedding::

  libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _)

Imports
------------

.. includecode:: code/DirectEmbedding.scala#imports

Row class and schema
------------------------
The schema description is currently provided as annotations on a case class which is used for holding rows.
We will later provide more flexible and customizable means of providing the schema information. 

.. includecode:: code/DirectEmbedding.scala#schema

Query
------------
Queryable takes an annotated case class as its type argument to formulate queries agains the corresponding table.

``_.price`` is of type Int here. The underlying, macro-based implementation takes care of that the shown arguments to map
and filter are not executed on the JVM but translated to database queries instead. 

.. includecode:: code/DirectEmbedding.scala#query

Execution
------------
To execute the queries we need to create a SlickBackend instance passing in the chosen database backend driver.

.. includecode:: code/DirectEmbedding.scala#result


Alternative direct embedding bound to a driver and session
------------------------------------------------------------------------
Using ImplicitQueryable, a queryable can be bound to a backend and session. The query can be further adapted and easily executed this way.

.. includecode:: code/DirectEmbedding.scala#implicitqueryable


Features
------------
The direct embedding currently only supports database columns, which can be mapped to either ``String, Int, Double``.

Queryable and ImplicitQueryable currently support the following methods:

``map, flatMap, filter, length``

The methods are all immutable meaning they leave the left-hand-side Queryable unmodified, but return a new Queryable incorporating the
changes by the method call. 

Within the expressions passed to the above methods, the following operators may be used:

``Any: ==``

``Int, Double: + < >``
  
``String: +``
  
``Boolean: || &&``

Other operators may type check and compile ok, if they are defined for the corresponding types.
They can however currently not be translated to SQL, which makes the query fail at runtime, for example: ``( coffees.map( c => c.name.repr ) )``.
We are evaluating ways to catch those cases at compile time in the future

Queries may result in sequences of arbitrarily nested tuples, which may also contain objects representing complete rows. E.g. 

.. includecode:: code/DirectEmbedding.scala#nesting

The direct embedding currently does not feature insertion of data. Instead we can use the :doc:`lifted embedding <lifted-embedding>` or :doc:`plain SQL queries <sql>`.

