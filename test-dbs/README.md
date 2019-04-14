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
- Verify that the path in `oracle.driverJar` is correct
- Set `oracle.enabled = true`

DB2
---

DB2 quick setup:
- Install [DB2 Express-C](http://www.ibm.com/software/data/db2/express/download.html)
- Remove all entries for JARs from the system CLASSPATH that were added by the DB2 installer
- Change the password in `db2.password` to the database password you specified during
  installation for the `db2admin` user
- Verify that the path in `db2.driverJar` is correct
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

SQL Server via JTDS quick setup:
- Install [SQL Server Express 2008 R2](http://www.microsoft.com/en-us/download/details.aspx?id=30438)
- In the SQL Server Configuration Manager, under "SQL Server Network Configuration", enable
  Named Pipes and change the path from `\\.\pipe\MSSQL$SQLEXPRESS\sql\query` to `\\.\pipe\sql\query`
- If your machine is on a Windows domain, you may need to set the domain in `sqlserver-jtds.domain`
- Set `sqlserver-jtds.enabled = true`

SQL Server via sqljdbc quick setup:
- Install [SQL Server Express 2008 R2](http://www.microsoft.com/express/Database/InstallOptions.aspx)
- Install [sqljdbc](http://www.microsoft.com/en-us/download/details.aspx?id=11774)
- Enter the correct path to `sqljdbc4.jar` in `sqlserver-sqljdbc.driverJar`
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
      driverJar = "file:///C:/oraclexe/app/oracle/product/11.2.0/server/jdbc/lib/ojdbc6.jar"
    }

    db2 {
      enabled = true
      password = secret
    }

    sqlserver-jtds {
      enabled = true
    }

    sqlserver-sqljdbc {
      enabled = true
      password = secret
      driverJar = "file:///C:/sqljdbc_4.0/enu/sqljdbc4.jar"
    }
