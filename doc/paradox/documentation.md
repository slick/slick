Markdown Documentation
======================

Slick documentation is written in Markdown and is compiled to HTML with
[Paradox](https://developer.lightbend.com/docs/paradox/current/). No external installation is required.

``` bash
# Generate HTML documentation in doc/target/
sbt paradox
```

In order to build the complete manual including scaladocs, use the `doc` target:

``` bash
sbt doc
```
