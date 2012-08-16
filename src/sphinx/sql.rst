Plain SQL Queries
=================

Sometimes you may need to write your own SQL code for an operation which is
not well supported at a higher level of abstraction. Instead of falling back
to the low level of JDBC_, you can use Slick's *Plain SQL* queries with a much
nicer Scala-based API.

Scaffolding
-----------

:ex:`jdbc/PlainSQL` demonstrates some features of the *Plain SQL* support. The
imports are different from what you're used to for the
:doc:`lifted embedding <lifted-embedding>` or
:doc:`direct embedding <direct-embedding>`:

.. includecode:: code/PlainSQL.scala#imports

First of all, there is no *Slick driver* being imported. The JDBC-based APIs
in Slick depend only on JDBC itself and do not implement any database-specific
abstractions. All we need for the database connection is ``Database``, plus
the ``threadLocalSession`` to simplify session handling.

The most important class for *Plain SQL* queries is
``scala.slick.jdbc.StaticQuery`` which gets imported as ``Q`` for more
convenient use.

The database connection is opened
:ref:`in the usual way <gettingstarted-dbconnection>`. We are also defining
some case classes for our data:

.. includecode:: code/PlainSQL.scala#setup

DDL/DML Statements
------------------

The simplest ``StaticQuery`` method is ``updateNA`` which creates a
parameterless (*NA = no args*) ``StaticQuery[Unit, Int]`` that is supposed to
return the row count from a DDL statement instead of a result set. It can be
executed the same way as a query that uses the :doc:`lifted embedding
<lifted-embedding>`. Here we are using ``.execute`` to run the query without
getting the results:

.. includecode:: code/PlainSQL.scala#updateNA

You can append a ``String`` to an existing ``StaticQuery`` object with ``+``,
yielding a new ``StaticQuery`` with the same types. The convenience method
``StaticQuery.u`` constructs an empty *update* query to start with (identical
to ``StaticQuery.updateNA("")``). We are using it to insert some data into the
``SUPPLIERS`` table:

.. includecode:: code/PlainSQL.scala#Q.u

Embedding literals into SQL code is generally not recommended for security and
performance reasons, especially if they are to be filled at run-time with
user-provided data. You can use the special concatenation operator ``+?`` to
add a bind variable to a query string and instantiate it with the provided
value when the statement gets executed:

.. includecode:: code/PlainSQL.scala#bindConcat

The SQL statement is the same for all calls:
``insert into coffees values (?,?,?,?,?)``

Query Statements
----------------

Similar to ``updateNA``, there is a ``queryNA`` method which takes a type
parameter for the rows of the result set. You can use it to execute a
*select* statement and iterate over the results:

.. includecode:: code/PlainSQL.scala#queryNA

In order for this to work, Slick needs to know how to read ``Coffee`` values
from a ``PositionedResult`` object. This is done with an implicit
``GetResult`` value. There are predefined ``GetResult`` implicits for the
standard JDBC types, for Options of those (to represent nullable columns) and
for tuples of types which have a ``GetResult``. For the ``Supplier`` and
``Coffee`` classes in this example we have to write our own:

.. includecode:: code/PlainSQL.scala#GetResult

``GetResult[T]`` is simply a wrapper for a function ``PositionedResult => T``.
The first one above uses the explicit ``PositionedResult`` methods ``getInt``
and ``getString`` to read the next ``Int`` or ``String`` value in the current
row. The second one uses the shortcut method ``<<`` which returns a value of
whatever type is expected at this place. (Of course you can only use it when
the type is actually known like in this constructor call.)

The ``queryNA`` method for parameterless queries is complemented by ``query``
which takes two type parameters, one for the query parameters and one for the
result set rows. Similarly, there is a matching ``update`` for ``updateNA``.
The execution methods of the resulting ``StaticQuery`` need to be called with
the query parameters, as seen here in the call to ``.list``:

.. includecode:: code/PlainSQL.scala#query

As an alternative, you can apply the parameters directly to the query, thus
reducing it to a parameterless query. This makes the syntax for parameterized
queries the same as for normal function application:

.. includecode:: code/PlainSQL.scala#applyQuery

String Interpolation
--------------------

In order to use the *string interpolation* prefixes ``sql`` and ``sqlu``,
you need to add one more import statement:

.. includecode:: code/PlainSQL.scala#imports.interpolation

As long as you don't want function-like reusable queries, interpolation is the
easiest and syntactically nicest way of building a parameterized query. Any
variable or expression injected into a query gets turned into a bind variable
in the resulting query string. (You can use ``#$`` instead of ``$`` to get the
literal value inserted directly into the query.) The result type is specified
in a call to ``.as`` which turns the object produced by the ``sql``
interpolator into a ``StaticQuery``:

.. includecode:: code/PlainSQL.scala#interpolate.sql

There is a similar interpolator ``sqlu`` for building *update* statements. It
is hardcoded to return an ``Int`` value so it does not need the extra ``.as``
call:

.. includecode:: code/PlainSQL.scala#interpolate.sqlu
