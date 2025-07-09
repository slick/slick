package slick.test

import org.junit.Test
import org.junit.Assert._
import slick.compiler.{CompilationCache, CacheKey}
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api._

class CompilationCacheBasicTest {
  
  @Test
  def testCacheKeyEquality(): Unit = {
    class TestTable(tag: Tag) extends Table[(Int, String)](tag, "test") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def * = (id, name)
    }
    val table = TableQuery[TestTable]
    
    // Same query should produce same cache key
    val query1 = table.filter(_.id === 1)
    val query2 = table.filter(_.id === 1)
    
    val key1 = CacheKey(query1.toNode, H2Profile.queryCompiler, "H2Profile")
    val key2 = CacheKey(query2.toNode, H2Profile.queryCompiler, "H2Profile")
    
    assertEquals("Same queries should produce same cache key", key1, key2)
    
    // Different queries should produce different cache keys
    val query3 = table.filter(_.id === 2)
    val key3 = CacheKey(query3.toNode, H2Profile.queryCompiler, "H2Profile")
    
    assertNotEquals("Different queries should produce different cache keys", key1, key3)
  }
  
  @Test
  def testGlobalCacheIntegration(): Unit = {
    // Clear global cache first
    CompilationCache.clear()
    
    class TestTable(tag: Tag) extends Table[(Int, String)](tag, "test") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def * = (id, name)
    }
    val table = TableQuery[TestTable]
    
    // Test that cached compilation works via the new API
    val query = table.filter(_.id === 1)
    
    // First compilation should be cache miss
    val compiled1 = H2Profile.queryCompiler.run(query.toNode, "H2Profile")
    
    // Second compilation should be cache hit (if cache is enabled)
    val compiled2 = H2Profile.queryCompiler.run(query.toNode, "H2Profile")
    
    if (CompilationCache.isEnabled) {
      // Both should produce the same result
      assertEquals("Cached compilation should produce same result", compiled1.tree, compiled2.tree)
      
      // Cache should have some hits
      val stats = CompilationCache.getStats
      assertTrue("Cache should have at least one hit", stats.hits > 0)
    }
  }
  
  @Test
  def testCacheStats(): Unit = {
    // Clear global cache first
    CompilationCache.clear()
    
    class TestTable(tag: Tag) extends Table[(Int, String)](tag, "test") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def * = (id, name)
    }
    val table = TableQuery[TestTable]
    
    if (CompilationCache.isEnabled) {
      val initialStats = CompilationCache.getStats
      
      // Compile a query
      val query = table.filter(_.id === 1)
      H2Profile.queryCompiler.run(query.toNode, "H2Profile")
      
      val afterFirstCompile = CompilationCache.getStats
      assertTrue("Cache misses should increase", afterFirstCompile.misses > initialStats.misses)
      
      // Compile same query again
      H2Profile.queryCompiler.run(query.toNode, "H2Profile")
      
      val afterSecondCompile = CompilationCache.getStats
      assertTrue("Cache hits should increase", afterSecondCompile.hits > afterFirstCompile.hits)
    }
  }
  
  @Test
  def testCacheSize(): Unit = {
    // Clear global cache first
    CompilationCache.clear()
    
    class TestTable(tag: Tag) extends Table[(Int, String)](tag, "test") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def * = (id, name)
    }
    val table = TableQuery[TestTable]
    
    if (CompilationCache.isEnabled) {
      val initialSize = CompilationCache.size
      
      // Compile different queries
      val query1 = table.filter(_.id === 1)
      val query2 = table.filter(_.id === 2)
      
      H2Profile.queryCompiler.run(query1.toNode, "H2Profile")
      H2Profile.queryCompiler.run(query2.toNode, "H2Profile")
      
      val finalSize = CompilationCache.size
      assertTrue("Cache size should increase", finalSize > initialSize)
    }
  }
}