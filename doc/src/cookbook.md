# Cookbook {index="cookbook;FAQ"}

This section of the manual is for solutions to common questions.

## Mapping more than 22 fields {index="22 fields"}

### Problem
You have a table with more than 22 fields, and the Scala 2 compiler has told you:

```text
too many elements for tuple: 23, allowed: 22
```

### Solution
Switch from using a tuple in your default projection to using an HList.

First, add the HList imports:

```scala src=../code/Cookbook.scala#imports22
```

Your case class and table definition are as unchanged,
but your default projection ( the ``*`` method) changes:

```scala src=../code/Cookbook.scala#example22
```

## Track the Number of Query Compilations

### Problem

Query compilation can be expensive. 
We can use the `Compiled` construct to avoid re-compiling queries. 
However, it can also be easy to accidentally forget to use this construct or revert its usage.
When this happens in a high-traffic deployment, query times and CPU usage can increase drastically.

To identify this type of regression, we'd like to track the number of query compilations.
We might then expose this count as an application metric and setup an alert which triggers
when an abnormally high number of query compilations occur.

### Solution

To track the number of query compilations, we can override the `computeQueryCompiler` method in our profile. 
The new `QueryCompiler` will have an additional phase, which simply increments a counter.


```scala src=../code/Cookbook.scala#exampleTrackNumberOfQueryCompilations
```