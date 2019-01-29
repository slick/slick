Markdown Documentation
======================

Slick documentation is written in CommonMark (Markdown) and is compiled to HTML with
[Ornate](https://github.com/szeiger/ornate) via the `sbt-ornate` plugin. No external installation is required.

``` bash
# Generate HTML documentation in doc/target/
sbt ornate
```

In order to build the complete manual including scaladocs, use the `doc` target:

``` bash
sbt doc
```
