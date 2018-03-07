package slick.test.profile

import org.junit.Assert._
import org.junit.Test
import slick.jdbc.{H2Profile, SQLiteProfile}

class ProfileNameTest {

  @Test def testProfileNames: Unit = {
    assertEquals("slick.jdbc.H2Profile$", H2Profile.toString)
    assertEquals("slick.jdbc.SQLiteProfile$", SQLiteProfile.toString)
  }
}
