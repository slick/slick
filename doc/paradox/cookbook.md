# Cookbook

This section of the manual is for solutions to common questions.

## Mapping more than 22 fields

<h3>Problem</h3>
You have a table with more than 22 fields, and the Scala 2 compiler has told you:

```text
too many elements for tuple: 23, allowed: 22
```

<h3>Solution</h3>
Switch from using a tuple in your default projection to using an HList.

First, add the HList imports:

@@snip [Cookbook.scala](../code/Cookbook.scala) { #imports22 }

Your case class and table definition are as unchanged,
but your default projection ( the ``*`` method) changes:

@@snip [Cookbook.scala](../code/Cookbook.scala) { #example22 }

## Track the Number of Query Compilations

<h3>Problem</h3>

Query compilation can be expensive. 
We can use the `Compiled` construct to avoid re-compiling queries. 
However, it can also be easy to accidentally forget to use this construct or revert its usage.
When this happens in a high-traffic deployment, query times and CPU usage can increase drastically.

To identify this type of regression, we'd like to track the number of query compilations.
We might then expose this count as an application metric and setup an alert which triggers
when an abnormally high number of query compilations occur.

<h3>Solution</h3>

To track the number of query compilations, we can override the `computeQueryCompiler` method in our profile. 
The new `QueryCompiler` will have an additional phase, which simply increments a counter.


@@snip [Cookbook.scala](../code/Cookbook.scala) { #exampleTrackNumberOfQueryCompilations }

## Distinguish DBIOActions by Effect type at runtime

<h3>Problem</h3>

When using a replicated database with a primary (read/write) node and one or more replica (read-only) nodes, we have to 
maintain separate database handles and run each `DBIOAction` on the appropriate database handle. If a `DBIOAction`
containing a write goes to the replica, it will fail. If a read-only `DBIOAction` goes to the primary, we lose the 
benefit of using a replicated database. In a large application, it's tedious to remember to use the right handle for
every `DBIOAction`, and it's easy to mix them up.

Ideally we would have a single method like `run` that takes a `DBIOAction` and determines whether to run it on the
primary or the secondary.

Slick provides a phantom `Effect` type parameter in each `DBIOAction`, but it's not immediately obvious how to use at 
runtime. For example, we cannot pattern-match on the `Effect` type parameter, as this parameter is erased at runtime 
due to type erasure. 

<h3>Solution</h3>

This solution shows one way that we can introspect the `Effect` type in order to determine whether the `DBIOAction`
should go to the primary or the secondary. We define a type class called `EffectInfo.ReadOnly`, which has a single 
member `isReadOnly: Boolean`. We provide instances of the type class for the various subtypes of `Effect`, such that 
`isReadOnly` is true only for `Effect` types `Read` and `Read with Transactional`. For any other subtypes, `isReadOnly` 
is false. We implement a `run` method which takes a `DBIOAction` and an implicit instance of the `EffectInfo.ReadOnly`
type class for the `DBIOAction`'s `Effect` type. We summon the type class instance and check the `isReadOnly` member 
to determine whether the given `DBIOAction` should go to the primary or to the secondary.

@@snip [Cookbook.scala](../code/Cookbook.scala) { #exampleDistinguishDBIOActionsByEffectTypeAtRuntime }
