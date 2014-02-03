Schema code generation
=============================================

The Slick code generator is a convenient tool for working
with an existing or evolving database schema. It can be run
stand-alone or integrated into you sbt build for creating all
code Slick needs to work.

Overview
--------
By default, the code generator generates Table classes, corresponding TableQuery values, which
can be used in a collection-like manner, as well as case classes for holding complete
rows of values. For Tables with more than 22 columns the generator automatically switches
to Slick's experimental HList implementation for overcoming Scala's tuple size limit. (If needed use ``HCons`` instead of ``::`` as a type contructor in Scala <= 2.10.3 for now due to performance issues during compilation.)

The implementation is ready for practical use, but since it is new in
Slick 2.0 we consider it experimental and reserve the right to remove features
without a deprecation cycle if we think that it is necessary. It would be only
a small effort to run an old generator against a future version of Slick though,
if necessary, as it's implementation is rather isolated from the rest of Slick.
We are interested in hearing about people's experiences with
using it in practice.

Parts of the generator are also explained in our `talk at Scala eXchange 2013 <http://slick.typesafe.com/docs/#20131203_patterns_for_slick_database_applications_at_scala_exchange_2013>`_.

Standalone use
---------------------------------------
Slick's code generator comes with a default runner that can be used from the command line or from Java/Scala. You can simply execute

   .. includecode:: code/CodeGenerator.scala#default-runner

and provide the following values

* **slickDriver** Fully qualified name of Slick driver class, e.g. *"scala.slick.driver.H2Driver"*
* **jdbcDriver** Fully qualified name of jdbc driver class, e.g. *"org.h2.Driver"*
* **url** jdbc url, e.g. *"jdbc:postgresql://localhost/test"*
* **outputFolder** Place where the package folder structure should be put
* **pkg** Scala package the generated code should be places in

Integrated into sbt
-------------------
The code generator can be run before every compilation or manually.
An example project showing both can be `found here <https://github.com/slick/slick-codegen-example/tree/master>`_.

Generated Code
--------------
By default, the code generator places a file ``Tables.scala`` in the given folder in a subfolder corresponding
to the package. The file contains an ``object Tables`` from which the code
can be imported for use right away. Make sure you use the same Slick driver.
The file also contains a ``trait Tables`` which can be used in the cake pattern.

Warning
-------
When using the generated code, be careful **not** to mix different database drivers accidentally. The default ``object Tables`` uses the driver used during code generation. Using it together with a different driver for queries will lead to runtime errors. The generated ``trait Tables`` can be used with a different driver, but be aware, that this is currently untested and not officially supported. It may or may not work in your case. We will officially support this at some point in the future.

Customization
-------------
The generator can be flexibly customized by overriding methods to programmatically
generate any code based on the data model. This can be used for minor customizations
as well as heavy, model driven code generation, e.g. for framework bindings (Play,...),
other data-related, repetetive sections of applications, etc.

`This example <https://github.com/slick/slick-codegen-customization-example/tree/master>`_
shows a customized code-generator and how to setup
up a multi-project sbt build, which compiles and runs it
before compiling the main sources.

The implementation of the code generator
is structured into a small hierarchy of sub-generators responsible
for different fragments of the complete output. The implementation of each
sub-generator can be swapped out for a customized one by overriding the corresponding
factory method. SourceCodeGenerator contains a factory method Table, which it uses to
generate a sub-generator for each table. The sub-generator Table in turn contains
sub-generators for Table classes, entity case classes, columns, key, indices, etc.
Custom sub-generators can easily be added as well.

Within the sub-generators the relevant part of the Slick data model can
be accessed to drive the code generation.

Please see the :api:`api documentation <scala.slick.model.codegen.SourceCodeGenerator>` for info
on all of the methods that can be overridden for customization.

Here is an example for customizing the generator:
   .. includecode:: code/CodeGenerator.scala#customization
