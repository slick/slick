Date and Time types {index=datetime}
=======
The [java.time.* types ](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html) that were introduced in Java 8, are well thought out and comprehensive. Mapping these into the disparate date and time
types implemented by the different database backends and mediated through the various jdbc implementations is tricky.

There are two principles that have been applied in mapping the java.time types into database types.

1. The values should be unchanged by a roundtrip to and from the database.
That means that you get back the same value that was stored, irrespective of the configuration of the
database or the JVM.

2. It should be clear to you as a user of the database what those values are. This may be with an appropriate database type, if one is available. If not, a varchar with a string representation of the value will be used. Standard ISO representation will be used (e,g, 2007-12-03T10:15:30.00Z for an Instant) and as such the values are sortable and comparable.

The different java.time types have important characteristics. E.g `Instant`s need to be continuous and always increasing,
with a larger value being later on the timeline.

Daylight savings times (DST) is where a lot of problems can occur. Without explicit offsets or timezones, then the times when the DST shifts take effect, can leave unrepresentable gaps.
The timestamp (without time zone) database type is handled different in different db backends (and jdbc implementations). Postgres, for example, stores timestamps as UTC and so they are approproriate to be the datatype for Instants. For databases other than Postgres, depending on timezone configurations in the database backend and on the client JVM, DST may be applied
  to a timestamp without timezone.
Where timestamps with time zone are supported by the db backend, then writing Instants as timestamps with timezone
`UTC` or offset `+00:00` gives us the characteristics we need for an `Instant`.


One java.time type to be careful of is `LocalDateTime`, that behaves most like the legacy `java.sql.Timestamp` and is mapped into a Timestamp database type. For these, the exact behaviour for DST related can depend on the environment configuration of the database or the JVM. And so is the one type that can break the "roundtripping" rule if the value being stored falls into a DST gap for a given environment configuration.

For example, if your environment is configured to be `Europe/London`, and you try to write the `LocalDateTime` `2018-03-25T01:37:40` into a database that performs DST shifts, such as Oracle, it will come back from the db as `2018-03-25T02:37:40`. This may be the desired behaviour, and is what happens with some databases for a Timestamp without timezone type. If this isn't the desired behaviour, then using a more appropriate type, e.g. `Instant` or `ZonedDateTime` will not have a DST gap issues, irrespective of environment configuration.

If you are unhappy with the mappings that have been chosen, you can configure your own. See the [Schemas](schemas.md) page for how to do this.
