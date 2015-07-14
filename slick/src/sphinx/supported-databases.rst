Supported Databases
===================

* DB2 (via :doc:`slick-extensions <extensions>`)
* Derby/JavaDB
* H2
* HSQLDB/HyperSQL
* Microsoft SQL Server (via :doc:`slick-extensions <extensions>`)
* MySQL
* Oracle (via :doc:`slick-extensions <extensions>`)
* PostgreSQL
* SQLite

Other SQL databases can be accessed right away with a reduced feature set.
Writing a fully featured plugin for your own SQL-based backend can be achieved
with a reasonable amount of work. Support for other backends (like NoSQL) is
under development but not yet available.

The following capabilities are supported by the drivers. "Yes" means that a
capability is fully supported. In other cases it may be partially supported or
not at all. See the individual driver's API documentation for details.






.. csv-table:: Driver Capabilities (core drivers only)
   :header-rows: 1
   :file: capabilities.csv
