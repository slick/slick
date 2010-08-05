package com.novocode.squery.test

import org.junit.runner.{RunWith, JUnitCore}
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses

@RunWith(classOf[Suite])
@SuiteClasses(Array(
  classOf[SimpleTest],
  classOf[MainTest],
  classOf[UnionTest],
  classOf[TemplateTest],
  classOf[DriverTest],
  classOf[MapperTest],
  classOf[StatementParametersTest],
  classOf[MutateTest],
  classOf[ScalarFunctionTest],
  classOf[JoinTest],
  classOf[InsertTest],
  classOf[MiscTest],
  classOf[ForeignKeyTest],
  classOf[MetaTest],
  classOf[TablesInObjectTest]
))
class TestSuite

object TestSuite {
  def main(args: Array[String]) = JUnitCore.main(Array(classOf[TestSuite].getName):_*);
}
