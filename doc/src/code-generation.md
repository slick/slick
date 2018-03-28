Schema Code Generation
======================

The Slick code generator is a convenient tool for working
with an existing or evolving database schema. It can be run
stand-alone or integrated into your sbt build for creating all
code Slick needs to work.

Overview
--------
By default, the code generator generates `Table` classes, corresponding `TableQuery` values, which
can be used in a collection-like manner, as well as case classes for holding complete
rows of values. For tables with more than 22 columns the generator automatically switches
to Slick's experimental `HList` implementation for overcoming Scala's tuple size limit.

Parts of the generator are also explained in our
[talk at Scala eXchange 2013](http://slick.typesafe.com/docs/#20131203_patterns_for_slick_database_applications_at_scala_exchange_2013).

Standalone use
--------------
To include Slick's code generator use the published library. For sbt projects add following to your build definition -
`build.sbt` or `project/Build.scala`:

```scala expandVars=true
libraryDependencies += "com.typesafe.slick" %% "slick-codegen" % "{{version}}"
```

For Maven projects add the following to your `<dependencies>`:

```xml expandVars=true
<dependency>
  <groupId>com.typesafe.slick</groupId>
  <artifactId>slick-codegen_2.11</artifactId>
  <version>{{version}}</version>
</dependency>
```

> {.note}
> In the code examples below we assume the following imports:
>```scala src=../code/CodeGenerator.scala#imports
> ```
> If you're new to Slick, please start with the [Getting Started](gettingstarted.md) page.


Slick's code generator comes with a default runner that can be used from the command line or from Java/Scala. You can simply execute

```scala src=../code/CodeGenerator.scala#default-runner-uri
```

or

```scala src=../code/CodeGenerator.scala#default-runner
```

or

```scala src=../code/CodeGenerator.scala#default-runner-with-auth
```

and provide the following values

* `uri`: Config URL and/or fragment for path in typesafe config, e.g. `url#slick.db.default`
* `profile`: Fully qualified name of the profile class, e.g. `slick.jdbc.H2Profile`
* `jdbcDriver`: Fully qualified name of the JDBC driver class, e.g. `org.h2.Driver`
* `url`: JDBC url, e.g. `jdbc:postgresql://localhost/test`
* `outputFolder`: Place where the package folder structure should be put
* `pkg`: Scala package the generated code should be places in
* `user`: database connection user name
* `password`: database connection password
* `outputToMultipleFiles`: Boolean indicating if the generated output should be one big file or one file per table. Default is false.

Integrated into sbt
-------------------
The code generator can be run before every compilation or manually in [sbt].
An example project showing both can be [found here](https://github.com/slick/slick-codegen-example).

Generated Code
--------------
By default, the code generator places a file `Tables.scala` in the given folder in a subfolder corresponding
to the package. The file contains an `object Tables` from which the code
can be imported for use right away. Make sure you use the same profile.
The file also contains a `trait Tables` which can be used in the cake pattern.

If `outputToMultipleFiles` is set to true, the code generator will instead create a `trait` per table.
`Tables.scala` then stacks all the generated tables. The advantage of this, is that you avoid a potentially huge file

> {.warning}
> When using the generated code, be careful *not* to mix different profiles accidentally. The
> default `object Tables` uses the profile used during code generation. Using it together with a different
> profile for queries will lead to runtime errors. The generated `trait Tables` can be used with a
> different profile, but be aware, that this is currently untested and not officially supported. It may or
> may not work in your case. We will officially support this at some point in the future.

Customization
-------------
The generator can be flexibly customized by overriding methods to programmatically
generate any code based on the data model. This can be used for minor customizations
as well as heavy, model driven code generation, e.g. for framework bindings in [Play],
other data-related, repetitive sections of applications, etc.

[This example](https://github.com/slick/slick-codegen-customization-example)
shows a customized code-generator and how to setup
up a multi-project sbt build, which compiles and runs it
before compiling the main sources.

The implementation of the code generator is structured into a small hierarchy of sub-generators responsible
for different fragments of the complete output. The implementation of each sub-generator can be swapped out
for a customized one by overriding the corresponding factory method.
<codegenapi:slick.codegen.SourceCodeGenerator> contains a factory method Table,
which it uses to generate a sub-generator for each table. The sub-generator Table in turn contains
sub-generators for Table classes, entity case classes, columns, key, indices, etc.
Custom sub-generators can easily be added as well.

Within the sub-generators the relevant part of the Slick data model can
be accessed to drive the code generation.

Please see the [api documentation](codegenapi:slick.codegen.SourceCodeGenerator) for info
on all of the methods that can be overridden for customization.

Here is an example for customizing the generator:

```scala src=../code/CodeGenerator.scala#customization
```
