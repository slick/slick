Coming from SQL to Slick
=========================

Coming from JDBC/SQL to Slick is pretty straight forward in many ways. Slick can be considered as a drop-in replacement with a nicer API for handling connections and fetching results and a query language, which is integrated more nicely into Scala than writing queries as Strings. The main obstacle for developers coming from SQL to Slick seems to be the semantic differences of seemingly similar operations between SQL and Scala's collections API which Slick's API imitates. The following sections give a quick overview over the differences. For a more detailed explanations of Slick's API please refer to :doc:`queries <queries>` and the equivalent methods in the :scalaapi:`the Scala collections API <scala.collection.immutable.Seq>`.

Queries in comparison
--------------------------------

JDBC Query
___________

A jdbc query query could look like this

.. includecode:: code/SqlToScala.scala#jdbc

Slick plain SQL query
______________________

Slick gives us two choices how to write queries. One is SQL strings just like JDBC. This is useful if you either want to continue writing queries in SQL or if you need a feature not (yet) supported by Slick otherwise. Executing the previous query as plain SQL using Slick looks like this:

.. includecode:: code/SqlToScala.scala#SlickPlainSQL



Slick type-safe queries
_______________________

Slick's key feature are type-safe, compositional queries. Slick comes with a Scala-to-SQL compiler, which allows a (purely functional) sub-set of the Scala language to be compiled to SQL queries. Also available are a subset of the standard library and some extensions, e.g. for joins. The familiarity allows Scala developers to instantly write many queries against all supported relational databases with little learning required and without knowing SQL or remembering the particular dialect. Such Slick queries are compositional, which means that you can write and re-use fragments and functions to avoid repetitive code like join conditions in a much more practical way than concatenating SQL strings. The fact that such queries are type-safe not only catches many mistakes early at compile time, but also eliminates the risk of SQL injection vulnerabilities.

The same query written as a type-safe Slick query looks like this:

.. includecode:: code/SqlToScala.scala#SlickTypesafeQuery

A key benefit compared to SQL strings is, that you can easily add to query by calling more methods on it. E.g. ``query.filter(_.age > 18)`` returns another query which also filters. This allows to build libraries of queries, which reuse parts and are much more maintainable. You can abstract over join conditions, pagination, filters, etc.

.. Add a link to more info on this

It is important to note that Slick needs the type-information to type-check these queries. This type information closely corresponds to the database schema and is provided to Slick in the form of table classes. They can either be :doc:`auto-generated <code-generation>` or :doc:`written by hand <schemas>`.

Main obstacle: Semantic API differences
---------------------------------------------------------

Some methods of the Scala collections use different semantics than their SQL counter parts. This seems to be one of the main causes of confusion for people newly coming from SQL to Slick. An example for such a semantic difference is groupBy. SQL's group by can be seen as an operation that turns all columns that weren't part of the grouping key into collections of all the elements in a group. SQL requires the use of it's aggregation operations like ``avg`` to compute single values out of these collections. Here is an example.

.. includecode:: code/SqlToScala.scala#groupBySQL

The signature of groupBy in Scala is different and this seems to confuse some people coming from SQL. Scala's groupBy returns a Map from grouping key to Lists of the elements in each group. In order to get SQL-like collections of all values of a column within a particular group, we first need to map from the group to the particular column, which you can often see in Slick queries using groupBy in order to aggregate the values in a SQL-like style. The same query looks like this using Slick's collections-like api.

.. includecode:: code/SqlToScala.scala#groupBySlick

Since SQL requires us to aggregate grouped values, we require the same in Slick, which makes Slick's grouping syntax a bit more complicated than SQL's. We may lift that restriction at some point.

To understand how to write any query using Slick's type-safe api, it can be helpful to think about how to write it using ordinary Scala collections, because it is simply modeled after that.


Scala-to-SQL compilation during runtime
---------------------------------------------------------

Slick runs a Scala-to-SQL compiler to implement its type-safe query feature. The compiler runs at Scala run-time and it does take its time which can even go up to second or longer for complex queries. It can be very useful to run the compiler only once per defined query, e.g. at app startup, not once per execution over and over. :ref:`Compiled queries <compiled-queries>` allow you to cache the generated SQL for re-use.

Limitations
---------------------------------------------------------

When you use Slick extensively you will run into cases, where Slick's type-safe query language does not support a query operator or JDBC feature you may desire to use or produces non-optimal SQL code. There are several ways to deal with that.

Missing query operators
________________________________________________________

define in terms of others
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Slick is extensible to some degree, which means you can add some kinds of missing operators yourself. If the operator you desire is expressible using existing Slick operations you can simply write a Scala function or implicit class, that implements the operator as a method in terms of existing operators. Here we implement `squared` using multiplication.

.. includecode:: code/SqlToScala.scala#slickFunction

define using a database function
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you need a fundamental operator, which is not supported out-of-the-box you can add it yourself if it operates on scalar values. For example Slick currently does not have a `power` method out of the box. Here we are mapping it to a database function.

.. includecode:: code/SqlToScala.scala#dbFunction

More information can be found in the chapter about :ref:`Scalar database functions <scalar-db-functions>`.

You can however not add operators operating on queries using database functions. The Slick Scala-to-SQL compiler requires knowledge about the structure of the query in order to compile it to the most simple SQL query it can produce. It currently couldn't handle custom query operators in that context. There are some ideas how this restriction can be somewhat lifted in the future, but it still needs more investigation first.

An example for such operator is a MySQL index hint, which is not supported by Slick's type-safe api and it cannot be added by users. If you require such an operator you have to write your whole query using plain SQL. If the operator does not change the return type of the query you could use the workaround described in the following section.

Unsupported JDBC features
________________________________________________________

There are some JDBC feature Slick simply doesn't currently support. Some of these are OUT-Parameters, stored procedures returning tables, multiple result sets. Often you can still use them by simply accessing the JDBC connection underlying a Slick session. More info can be found in the section about :ref:`JDBC interop <jdbc-interop>`. 

Non-optimal SQL code
________________________________________________________

Slick generates SQL code and tries to make it as simple as possible. The algorithm doing that is not perfect and under continuous improvement. There are cases where the generated queries are more complicated than someone would write them by hand. This can lead to bad performance for certain queries with some optimizers and DBMS. For example, Slick occasionally generates unnecessary sub-queries. In MySQL <= 5.5 this easily leads to unnecessary table scans or indices not being used. The Slick team is working towards generating code better factored to what the query optimizers can currently optimize, but that doesn't help you now. To work around it you have to write the more optimal SQL code by hand. You can either run it as a Slick plain SQL query or you can use the following workaround, which allows you to simply swap out the SQL code Slick uses for a type-safe query.

.. includecode:: code/SqlToScala.scala#overrideSql

