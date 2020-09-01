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
