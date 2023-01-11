Test database configuration
===========================

You can create a file `testkit.conf` here (or override the location of that file with the system
property `slick.testkit-config`. The standard format for the configuration is HOCON but all formats
supported by [Typesafe Config](https://github.com/typesafehub/config) can be used.

The default configuration is always loaded from
[/testkit-reference.conf](../slick-testkit/src/main/resources/testkit-reference.conf) on the
classpath. You do not have to copy any of these settings into your own config file.

All embedded databases are enabled by default. The following external database systems require
extra configuration:

PostgreSQL
----------

PostgreSQL quick setup (tested with 9.2.4-1):
- Install PostgreSQL server with default options
- Set "postgres.user" (default: "postgres") and postgres.password (default: null) if necessary
- Set postgres.enabled = true

MySQL
-----

MySQL quick setup (tested with 5.7.25):
- Install MySQL community server with default options
- Set "mysql.user" (default: "root") and mysql.password (default: null) if necessary
- Set enabled = true

Oracle
------

Oracle quick setup:
- Install [Oracle Database Express Edition 11g Release 2](http://www.oracle.com/technetwork/products/express-edition/downloads/)
- Change password in `oracle.password` to the database password you specified during installation
- Copy the oracle jdbc driver jar into the `slick-testkit/lib` directory
- Set `oracle.enabled = true`

DB2
---

DB2 quick setup:
- Install [DB2 Express-C](http://www.ibm.com/software/data/db2/express/download.html)
- Remove all entries for JARs from the system CLASSPATH that were added by the DB2 installer
- Change the password in `db2.password` to the database password you specified during
  installation for the `db2admin` user
- Copy the DB2 jdbc driver jar into the `slick-testkit/lib` directory
- In the DB2 Command Line Processor, create the database for testing and
  grant permissions to the db2admin user:

```
    db2 => create database slicktst
    db2 => connect to slicktst
    db2 => grant dbadm on database to db2admin
```

- Set `db2.enabled = true`

Sample script for setting up the DB2 test database on a different partition (e.g. RAM disk):

    create database slicktst on 'r:'
    connect to slicktst
    create tablespace systoolspace in ibmcatgroup managed by automatic storage extentsize 4
    create user temporary tablespace systoolstmpspace in ibmcatgroup managed by automatic storage extentsize 4
    grant dbadm on database to db2admin

SQL Server
----------

SQL Server quick setup:
- Install [SQL Server Developer](https://www.microsoft.com/en-us/sql-server/sql-server-downloads)
- Enter the password for the `sa` user in `sqlserver-sqljdbc.password`
  (use SQL Server Management Studio to set a password if necessary)
- Ensure that the TCP transport on port 1433 is enabled (-> SQL Server Configuration Manager)
- Ensure that the `sa` user is allowed to log in, and that SQL Server accepts
  SQL Server authentication (not just Windows authentication) (-> SQL Server Management Studio)
- Set `sqlserver-sqljdbc.enabled = true`

Example configuration
=====================

    testkit.testDir = "r:/test-dbs"

    postgres {
      enabled = true
      user = me
    }

    mysql {
      enabled = true
      password = secret
    }

    oracle {
      enabled = true
      password = secret
    }

    db2 {
      enabled = true
      password = secret
    }

    sqlserver-sqljdbc {
      enabled = true
      password = secret
    }
