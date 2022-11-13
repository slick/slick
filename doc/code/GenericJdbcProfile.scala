//#genericJdbcProfile
package com.typesafe.slick.docs

import slick.jdbc.{JdbcActionComponent, JdbcProfile}

object GenericJdbcProfile extends JdbcProfile with JdbcActionComponent.MultipleRowsPerStatementSupport
//#genericJdbcProfile
