package slick.test.util

import org.junit.Test
import org.junit.Assert._
import slick.util.LoggingContext

class LoggingContextTest {

  @Test
  def testEmptyContext(): Unit = {
    val ctx = LoggingContext.empty
    assertTrue("Empty context should be empty", ctx.isEmpty)
    assertFalse("Empty context should not be non-empty", ctx.nonEmpty)
    assertEquals("Empty context formatting", "", ctx.formatForLogging)
  }

  @Test
  def testSingleValueContext(): Unit = {
    val ctx = LoggingContext.single("key", "value")
    assertFalse("Single value context should not be empty", ctx.isEmpty)
    assertTrue("Single value context should be non-empty", ctx.nonEmpty)
    assertEquals("Single value formatting", "[key=value] ", ctx.formatForLogging)
  }

  @Test
  def testContextFromKeyValuePairs(): Unit = {
    val ctx = LoggingContext("key1" -> "value1", "key2" -> "value2")
    assertEquals("Context should have correct values", Map("key1" -> "value1", "key2" -> "value2"), ctx.values)
    assertTrue("Context formatting should contain both pairs", 
      ctx.formatForLogging.contains("key1=value1") && ctx.formatForLogging.contains("key2=value2"))
  }

  @Test
  def testContextAddition(): Unit = {
    val ctx1 = LoggingContext("key1" -> "value1")
    val ctx2 = ctx1 + ("key2" -> "value2")
    
    assertEquals("Original context should be unchanged", Map("key1" -> "value1"), ctx1.values)
    assertEquals("New context should have both values", 
      Map("key1" -> "value1", "key2" -> "value2"), ctx2.values)
  }

  @Test
  def testContextMerge(): Unit = {
    val ctx1 = LoggingContext("key1" -> "value1", "shared" -> "original")
    val ctx2 = LoggingContext("key2" -> "value2", "shared" -> "updated")
    val merged = ctx1.merge(ctx2)
    
    assertEquals("Merged context should have all keys with precedence", 
      Map("key1" -> "value1", "key2" -> "value2", "shared" -> "updated"), merged.values)
  }

  @Test
  def testContextBatchAddition(): Unit = {
    val ctx = LoggingContext("existing" -> "value")
    val additional = Map("new1" -> "value1", "new2" -> "value2")
    val updated = ctx ++ additional
    
    assertEquals("Updated context should have all values",
      Map("existing" -> "value", "new1" -> "value1", "new2" -> "value2"), updated.values)
  }
}