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

MySQL quick setup (tested with 5.6.11.0):
- Install MySQL community server with default options
- Set "mysql.user" (default: "root") and mysql.password (default: null) if necessary
- Set enabled = true

Example configuration
=====================

    postgres {
      enabled = true
      user = me
    }

    mysql {
      enabled = true
      password = secret
    }
