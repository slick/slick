package slick.test.driver

import org.junit.Test
import org.junit.Assert._
import slick.driver.{SQLiteDriver, H2Driver}

class DriverNameTest {

  @Test def testDriverNames: Unit = {
    assertEquals("H2Driver", H2Driver.toString)
    assertEquals("SQLiteDriver", SQLiteDriver.toString)
  }
}
