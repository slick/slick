:tocdepth: 2

Upgrade guide
#################

Slick 2.0 from 1.x
------------------

Schema description
^^^^^^^^^^^^^^^^^^^^
In Slick 1.x it was common to describe your tables using singleton objects. Slick 1.x cloned the objects under the hood to distinguish different instances of the same table within the same query. But as we learned `Scala does not support this <https://issues.scala-lang.org/browse/SI-3764>`_.

Now table descriptions have to be classes that take an argument of type ``Tag`. Use

.. code-block:: scala

    // Slick 2.0
    object SomeTable(tag: Tag) extends Table[T](tag, "SOME_TABLE"){ ... }
    val SomeTable = TableQuery[SomeTable]
    // or:
    val SomeTable = TableQuery[SomeTable]( tag => SomeTable(Tag) )

instead of

.. code-block:: scala

  // Slick 1.x
  object SomeTable extends Table[T]("SOME_TABLE"){ ... }

See :ref:`Mapped Tables <mapped-tables>`.

withSession :Session

insert
^^^^^^^^^^^^^^^^^^^^
Slick 2.0 ignores primary key columns in inserts for convenience. Slick 1 required manually writing projections for this.






Query vs. TableQuery

threadlocalSession
^^^^^^^^^^^^^^^^^^^^




stichpunkte:

slick migration guide

https://groups.google.com/forum/#!topic/scalaquery/xNtPT6sexXI

MappedTypeMapper => MappedColumnType im cake

ExtendedProfile/BasicProfile => JdbcProfile

finders in table objects => TableQuery class

insert / insertForced

ForeignKey moved
