Schema Code Generation
======================

The Slick code generator is a convenient tool for working
with an existing or evolving database schema. It can be run
stand-alone or integrated into you sbt build for creating all
code Slick needs to work.

Overview
--------
By default, the code generator generates ``Table`` classes, corresponding ``TableQuery`` values, which
can be used in a collection-like manner, as well as case classes for holding complete
rows of values. For tables with more than 22 columns the generator automatically switches
to Slick's experimental ``HList`` implementation for overcoming Scala's tuple size limit. (In Scala
<= 2.10.3 use ``HCons`` instead of ``::`` as a type contructor due to performance issues during compilation, which are fixed in 2.10.4 and later.)

Parts of the generator are also explained in our `talk at Scala eXchange 2013 <http://slick.typesafe.com/docs/#20131203_patterns_for_slick_database_applications_at_scala_exchange_2013>`_.

Standalone use
---------------------------------------
To include Slick's code generator use the published library. For sbt projects add following to your build definition -
``build.sbt`` or ``project/Build.scala``:

.. parsed-literal::
  libraryDependencies += "com.typesafe.slick" %% "slick-codegen" % "|release|"

For Maven projects add the following to your ``<dependencies>``:

.. parsed-literal::
  <dependency>
    <groupId>com.typesafe.slick</groupId>
    <artifactId>slick-codegen_2.10</artifactId>
    <version>\ |release|\ </version>
  </dependency>

Slick's code generator comes with a default runner that can be used from the command line or from Java/Scala. You can simply execute

.. includecode:: code/CodeGenerator.scala#default-runner

or

.. includecode:: code/CodeGenerator.scala#default-runner-with-auth

and provide the following values

* **slickDriver** Fully qualified name of Slick driver class, e.g. *"slick.driver.H2Driver"*
* **jdbcDriver** Fully qualified name of jdbc driver class, e.g. *"org.h2.Driver"*
* **url** jdbc url, e.g. *"jdbc:postgresql://localhost/test"*
* **outputFolder** Place where the package folder structure should be put
* **pkg** Scala package the generated code should be places in
* **user** database connection user name
* **password** database connection password

Integrated into sbt
-------------------
The code generator can be run before every compilation or manually in sbt_.
An example project showing both can be `found here <https://github.com/slick/slick-codegen-example>`_.

Generated Code
--------------
By default, the code generator places a file ``Tables.scala`` in the given folder in a subfolder corresponding
to the package. The file contains an ``object Tables`` from which the code
can be imported for use right away. Make sure you use the same Slick driver.
The file also contains a ``trait Tables`` which can be used in the cake pattern.

.. warning::
   When using the generated code, be careful **not** to mix different database drivers accidentally. The
   default ``object Tables`` uses the driver used during code generation. Using it together with a different
   driver for queries will lead to runtime errors. The generated ``trait Tables`` can be used with a
   different driver, but be aware, that this is currently untested and not officially supported. It may or
   may not work in your case. We will officially support this at some point in the future.

Customization
-------------
The generator can be flexibly customized by overriding methods to programmatically
generate any code based on the data model. This can be used for minor customizations
as well as heavy, model driven code generation, e.g. for framework bindings in Play_,
other data-related, repetitive sections of applications, etc.

`This example <https://github.com/slick/slick-codegen-customization-example>`_
shows a customized code-generator and how to setup
up a multi-project sbt build, which compiles and runs it
before compiling the main sources.

The implementation of the code generator is structured into a small hierarchy of sub-generators responsible
for different fragments of the complete output. The implementation of each sub-generator can be swapped out
for a customized one by overriding the corresponding factory method.
:codegenapi:`SourceCodeGenerator <slick.codegen.SourceCodeGenerator>` contains a factory method Table,
which it uses to generate a sub-generator for each table. The sub-generator Table in turn contains
sub-generators for Table classes, entity case classes, columns, key, indices, etc.
Custom sub-generators can easily be added as well.

Within the sub-generators the relevant part of the Slick data model can
be accessed to drive the code generation.

Please see the :codegenapi:`api documentation <slick.codegen.SourceCodeGenerator>` for info
on all of the methods that can be overridden for customization.

Here is an example for customizing the generator:

.. includecode:: code/CodeGenerator.scala#customization
