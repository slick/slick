Sphinx Documentation
====================

Building
--------

Slick documentation is written in reStructuredText and is compiled by Sphinx via the sbt-site plugin.

.. code-block:: bash

  # Generate HTML documentation in slick/target/sphinx/html/
  sbt sphinx:generateHtml

For more details on Sphinx installation and usage in Scala projects, see the `Akka Sphinx`_ documentation.