Coming from ORM to Slick
========================
Slick is not an object-relational mapper (ORM) like Hibernate or others. Slick is a data persistence solution like ORMs and naturally shares some concepts, but it also has significant differences. This chapter explains the differences in order to get the best out of Slick and avoid confusion for those familiar with ORMs. We explain how Slick manages to avoid many of the problems often referred to as the object-relational impedance mismatch.

<show table diagram>

ORMs are a natural approach when using databases from object-oriented languages. They persist object-graphs to relational databases and try to allow working with them partly as if they were completely in memory. Objects can be modified, associations can be changed and the object graph can be traversed. In practice this is not exactly easy to achieve due to the so called object-relational impedance mismatch. It makes ORMs hard to implement and often complicated to use for more than simple cases and if performance matters. Slick in contrast does not persist an object-graph. It is inspired by SQL and the relational model and mostly just maps their concepts to the most closely corresponding type-safe Scala features. This leads to a restricted, immutable, purely-functional subset of Scala, in which database tables can be accessed very much like immutable collections.

In practice ORMs often suffer from conceptual problems of what they try to achieve, from mere problems of the implementations and from mis-use, because of their complexity. In the following we look at many features of ORMs and what you would use with Slick instead. We'll first look at how to work with the object graph. We then look at a series of particular features and use cases and how to handle them with Slick.

Navigating the object graph
----------------------------

Using plain old method calls
______________________________________________
This chapter could also be called strict vs. lazy or imperative vs. declarative. One common feature ORMs provide is using a persisted object graph just as if it was in-memory. And since it is not, artifacts like members or related objects are usually loaded ad-hoc only when they are needed. To make this happen, ORMs implement or intercept method calls, which look like they happen in-memory, but instead execute database queries as needed to return the desired results. Let's look at an example using a hypothetical ORM:

val people: Seq[Person] = PeopleFinder.findByIds(Seq(2,99,17,234))
val addresses: Seq[Address] = people.map(_.address)

How many database roundtrips does this require? In fact reasoning about this question for different code is one of the things you need to devote the most time to when learning the collections-like API of an ORM. What usually happens is, that the ORM would do an immediate database round trip for ``findByIds`` and return the resulting people. Then ``map`` would be a Scala List method and ``.map(_.address)`` accesses the ``address`` of each person. An ORM would witness the ``address`` accesses one-by-one not knowing upfront that they happen in a loop. This often leads to an additional database round-trip for each person, which is not ideal (n+1 problem), because database round-trips are expensive. To solve the problem, ORMs often provide means to work around this, by basically telling them about the future, so they can aggregate many upcoming calls into fewer more efficient ones.

val people: Seq[Person] = PeopleFinder.findByIds(Seq(2,99,17,234)).prefetch(_.address) // tell the ORM to load all related addresses together
val addresses: Seq[Address] = people.map(_.address)

Here the prefetch method instructs the hypothetical ORM to load all addresses immediately with the people, often in only one or two database roundtrips. The addresses are then stored in a cache many ORMs maintain. The later ``.map(_.address)`` call could then be fully served from the cache. Of course this is redundant as you basically need to provide the mapping to addresses twice and if you forget to prefetch you will have poor performance. How you specify the pre-fetching rules depends on the ORM, often using external configuration or inline like here.

Slick works differently. To do the same in Slick you would write the following. The type annotations are optional but shown here for clarity.

val peopleQuery: Query[PersonTable,Person,Seq] = People.filter(_.id inSeq(Seq(2,99,17,234)))
val addressesQuery: Query[AddressTable,Address,Seq] = people.flatMap(_.address)

As we can see it looks very much like collection operations but the values we get are of type ``Query``. They do not store results, only a plan of the operations that are needed to create a SQL query which produces the results when needed. No database roundtrips happen at all in our example. To actually fetch results, you can use the ``.run`` method on one of our values.

val addresses: Seq[Address] = addressesQuery.run

A single query is executed and the results returned. This makes database roundtrips very explicit and easy to reason about. Achieving few database roundtrips is easy.

As you can see with Slick we do not navigate the object graph (i.e. results) directly, but by composing queries instead. We just compose queries instead, which are just place-holder values for database potential database round-trip yet to happen. We can lazily compose queries until they describe exactly what we need and then use a single ``.run`` call for execution. The problem explained above makes navigating the object graph directly not a very useful feature in many cases. As a consequence ORMs often offer a declarative query language as an alternative.

Query languages
_______________________
ORMs often come with query languages like Hibernate's HQL or Criteria Queries. Similar to SQL or Slick, they allow expressing queries yet to happen and make execution explicit. All of them are comprehension-like declarative query languages.

String based embeddings
^^^^^^^^^^^^^^^^^^^^^^^^
Quite commonly,  these languages, for example HQL, but also SQL are embedded into programs as Strings. Here is an example for HQL.

val hql: String = "FROM Person p WHERE p.id in (:ids)";
val q: Query = s.createQuery(hql);
q.setParameterList("ids", Array(2,99,17,234));

Strings are a very simple way to embed an arbitrary language and in many programming languages the only way without changing the compiler, for example in Java. While simple, this kind of embedding has significant limitations.

On issue is that, that tools often have no knowledge about the embedded language and treat queries as ordinary Strings. The compiler or interpreter of the host languages does not detect syntactical mistakes upfront or if the query produces a different type of output than expected. Also IDEs often do not provide syntax highlighting, code completion, inline error hints, etc.

More importantly, re-use is very hard, when it comes to queries embedded as Strings. You would need to compose Strings in order to re-use certain parts. As an exercise, try to make the part of our above HQL example, that filters by the ids re-useable, so we can use it for table person as well as address. It is really cumbersome.

While strings may be the most flexible way of embedding queries into Java and some other languages, Scala is more flexible as we will see in the next sections.

Method based apis
^^^^^^^^^^^^^^^^^^^^^
Instead of getting the ultimate flexibility for the embedded language, an alternative approach is to go with the extensibility features of the host language and use those. Object-oriented languages like Java and Scala allow extensibility throw the definition of api's consisting of object objects and methods. Hibernate's Criteria Queries use this concept and so does Slick.

val id = Property.forName("id");
val q = session.createCriteria(Person.class)
                      .add( id in Array(2,99,17,234) )

This allows the host language tools some limited understanding about the embedded language providing better support for the features mentioned earlier.

It also makes queries compositional. Factoring out filtering by ids becomes easy:

def byIds(c: Criteria, ids: Array[Int]) = c.add( id in ids )

val c = byIds(
  session.createCriteria(Person.class),
  Array(2,99,17,234)
)

Of course ids are a trivial example, but this becomes very useful for more complex examples.

Java APIs like Hibernate Criteria Queries do no use Scala's operator overloading capabilities. This can leads to more cumbersome and less familiar code when expressing queries. Let's filter for all people younger 5 or older than 65 for example.

val age = Property.forName("age")
val q = session.createCriteria(Person.class)
                      .add(
			Restrictions.disjunction
				.add(age lt 5)
				.add(age gt 65)
			)

With Scala's operator overloading we can do better and that's what Slick uses. The same query in Slick would look like this:

val q = People.filter(p => p.age < 5 || p.age > 65)

There are some limitations to Scala's overloading capabilities that affect Slick. Instead of ``==`` one has to use ``===`` in Slick queries. Also it is not possible to overload ``if`` expressions. Instead Slick comes with a small DSL for SQL case expressions.

As already mentioned, we are working with placeholder values, merely describing the query, not executing it. Here's the same expression again with added toe annotation to allow us looking behind the scenes a bit:

val q = (People: Query[PersonTable, Person]).filter(
	(p: PersonTable) => 
		(
			((p.age: Column[Int]) < 5 || p.age > 65)
			: Column[Boolean]
		)
)

``Query`` marks collection-like query expressions, e.g. a whole table. ``PersonTable`` is the Slick Table subclass defined for table person. In this context the name of the type may be misleading as it is conceptually used as a prototype for a row here. It has members of type Column representing the individual columns. Expressions based on these columns result in other expressions of type Column. Here we are using Column[Int]'s to compute a Column[Boolean], which we use as the filter expression. Internally, Slick builds a tree from this, which represents the operations and is used to produce the corresponding SQL code. We often call the process of building up expression trees encapsulated in the place-holder values as lifting, which is why often call this query interface the lifted embedding in Slick. 

It is important to note that Scala allows to be very type-safe here. E.g. Slick supports a method ``.substring`` for Column[String] for not for Column[Int]. This is impossible in Java and Java APIs like Criteria Queries, but possible in Scala using type-parameter based method extensions via implicits. This allows tools like the Scala compiler and IDEs to understand the code much more precisely and offer better checking and support.

A nice property of a Slick-like query language is, that it can be used with Scala's comprehension syntax, which is just Scala-builtin syntactic sugar for collections operations. The above example could be written like:

for( p <- People if p.age < 5 || p.age > 65 ) yield p

Scala's comprehension syntax looks much like SQL or ORM query languages. It however lacks syntactic support for some constructs like sorting and grouping, for which one has to use the method-based api, e.g.

( for( p <- People if p.age < 5 || p.age > 65 ) yield p ).orderBy(_.name)

Despite the syntactic limitations the comprehension syntax is convenient when dealing with multiple inner joins.

It is important to note that the problems of method-based query apis like Criteria Queries described above are not a conceptual limitation of ORM query languages but merely an artifact of many ORMs being Java frameworks. In principle, a Scala ORMs could offer a query language just like Slick's and they should. Comfortably compositional queries allow for a high degree of code re-use. They seem to be Slick's favorite feature for many developers.

Macro-based embeddings
^^^^^^^^^^^^^^^^^^^^^^^^^
Scala macros offer another approach. They can be used to check queries embedded as Strings at compile time. They can also be used to translate Scala code written without Query and Column place holder types to SQL. Both approaches are being prototyped and evaluated for Slick but are not ready for prime-time yet. There are other database libraries out there that already use this approach for their query language.


Mapping configuration
---------------------------------------
In ORMs you often provide your mapping specification in a configuration file. In Slick you provide it as Scala types, which are use to type check Slick queries. More information can be found here in the chapter about mapping TODO link. A difference is the the Slick mapping is conceptually very simple. It simple describes database tables and optionally maps rows to case classes or anything else using arbitrary factories and extractors. It does contain information about foreign keys, but nothing else about relationships or other patterns. These are mapped using re-usable queries fragments instead. More in the following section about Relationships.

Query granularity
---------------------
With ORMs it is not uncommon to treat object or full rows as the smallest granularity when loading data. This is not necessarily a limitation of the frameworks, but a habit of using them. With Slick it is very much encouraged to only fetch the data you actually need. While you can map rows to classes with Slick, it is often more efficient to not use that feature, but to restrict your query. If you only need a person's name and age, just map to those and return them as a tuple.

People.map(p => (p.name, p.age))

This allows you to be very precise about what data is actually transferred.

Reads (caching)
---------------------
Slick doesn't cache query results. Working with Slick is like working with JDBC in this regard. Many ORMs come with read and write caches. Caches are side-effects. They can be hard to reason about. It can be tricky to manage cache consistency and lifetime.

PeopleFilter.getById(5)

This call may be served from the database or from a cache. It is not clear at the call site what the performance is. In Slick is is very clear, executing a query leads to a database roundtrip using an object does not.

People.filter(_.id === 5).run

Slick returns a consistent, immutable snapshot of a fraction of the database at that point in time. If you need consistency over multiple queries, use transactions.

Writes (and caching)
----------------------------------------------------
Writes in many ORMs require write caching to be performant.

val person = PeopleFilter.getById(5)
person.name = "Chris"
person.location = "Switzerland"
session.save()

Here our hypothetical ORM records changes to the object and the save methods syncs changed back to the database in a single roundtrip rather than one per member. In Slick you would do the following instead:

val personQuery = People.filter(_.id === 5)
personQuery.map(p => (p.name,p.location)).update("Chris","Switzerland")

Slick embraces immutability. Rather than modifying individual members of objects one after the other, you state all modifications at once and Slick creates a single database roundtrip from it without using a cache. New Slick users seem to be often confused by this syntax, but it is actually very neat. Slick unifies the syntax for queries, inserts, updates and deletes. Here ``q`` is just a query. We could use it to fetch data. But instead, we can also use it to update the columns specified by the query. Or we can use it do delete the rows

personQuery.delete // deletes person with id 5

For inserts, we insert into the query, that resembles the whole table and can select individual columns in the same way.

People.map(_.name).insert("Chris")


Relationships
--------------------
ORMs usually provide built-in support for 1-to-many and many-to-many relationships. ORMs hard-code support for them and provide some kind of configuration options to specify them. In SQL on the other hand you would specify them using join in every single query. While Slick works more like SQL, it is compositional and supports abstraction. With Slick you can abstract over relationships or anything else naturally like you abstract over other Scala code. There is not need to hard-code support for certain use cases in Slick and indeed there isn't. You can re-use arbitrary use cases by writing functions. E.g.

implicit class PersonExtensions[C[_]](q: Query[PersonTable, Person, C]) = {
  // specify mapping of relationship to address
  def withAddress = q.join(Address).on(_.addressId === _.id)
}

val chrisQuery = People.filter(_.id === 4234)
val stefanQuery = People.filter(_.id === 6455)

val chrisWithAddress: (Person, Address) = chrisQuery.withAddress.run
val stefanWithAddress: (Person, Address) = stefanQuery.withAddress.run

This way you can abstract over arbitrary use cases, e.g. the common 1-n or n-n relationships or even relationships spanning over multiple tables, relationships with additional discriminators, polymorphic relationships, etc.

A common question for new Slick users is how they can follow a relationships on a result. In an ORM you could do something like this:

val chris: Person = PeopleFilter.byId(4234)
val address: Address = chris.address

Also already explained in the section about navigating the object graph, Slick does not allow navigation as if data was in memory, because that makes it non-obvious when database roundtrips happen and can easily lead to too many round-trips. Slick is explicit about it. In Slick you would do this instead:

val chrisQuery: Query[PersonTable,Person] = People.filter(_.id === 4234)
val addressQuery: Query[AddressTable,Address] = chrisQuery.withAddress.map(_._2)
val address = addressQuery.first

If you leave out the type annotation and some intermediate vals it is very clean. And it is very clear where database roundtrips happen.

A variant of this question Slick new comers often ask is how they can our example to this in Slick:

case class Address( … )
case class Person( name: String, …, address: Address )

But this hard-codes that a Person cannot be loaded from the database without the address. This does't fit to Slick's philosophy of giving you fine-grained control over what you load exactly. With Slick it is advised to map one table to a tuple or case class without them having object references to related objects. Instead you can write a function that joins two tables and returns them as a tuple or association case class, providing an association externally, not strongly tied one of the classes.

case class PersonWithAddress(person: Person, address: Address)
People join Address on (_.addressId === _.id) map PersonWithAddress.tupled

An alternative approach is giving your classes Option-typed members referring to related objects, where None means that the related object has not been loaded yet.

case class Person( name: String, …, address: Option[Address] = None ){
case class Address( … )

People.join(Address).on(_.addressId === _.id).run.map{ case (p,a) => p.copy(address=a) }

Of course you can parameterize this snippet and put it into a function for re-use. Having the additional Option field adds sightly more overhead to your Table mapping in order to fill it with None by default and ignore it on inserts. And in the end, if you use Slick to select exactly the fields you need in every concrete use case, this kind of mapping may be less useful than it may seem at first.

Modifying relationships
________________________
When manipulating relationships with ORMs you usually work on mutable collections of associated objects and inserts or remove related objects. Similar to what the Writes (and caching) section describes, ORMs usually record changes and commit them at once using a ``save`` method. Slick embraces immutability, explicit execution and avoids caching complexities. Instead of changing mutable collections, you work with foreign keys, just like in SQL. Changing relationships means updating foreign key fields to new ids, just like any other field. As a bonus this allows establishing and removing associations with objects that have not been loaded into memory. Having their ids is sufficient.

Related talks
--------------------------
The Scala Days 2013 and Scala eXchange 2013 talks cover related topics among other things: http://slick.typesafe.com/docs/
