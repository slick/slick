package slick

import java.sql.{PreparedStatement, ResultSet}

import slick.relational.ResultConverterDomainImpl

/** Contains the abstract `JdbcProfile` and related code. This includes all JDBC-related code,
  * facilities for <em>Plain SQL</em> queries, and JDBC-specific profile components. */
package object jdbc {

  type JdbcResultConverterDomain = ResultConverterDomainImpl[ResultSet, PreparedStatement, ResultSet]
}
