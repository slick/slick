package slick.test

import slick.compiler.CompilationCache
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api._

/** Simple demo to show compilation cache working */
object CompilationCacheDemo {
  
  def main(args: Array[String]): Unit = {
    println("=== Slick Compilation Cache Demo ===")
    println()
    
    // Define a simple table
    class Users(tag: Tag) extends Table[(Int, String)](tag, "users") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name")
      def * = (id, name)
    }
    val users = TableQuery[Users]
    
    // Clear cache for clean test
    CompilationCache.clear()
    
    println(s"Cache enabled: ${CompilationCache.isEnabled}")
    println(s"Initial cache size: ${CompilationCache.size}")
    println()
    
    // Compile same query multiple times
    val query = users.filter(_.id === 1)
    
    println("Compiling same query 5 times...")
    
    (1 to 5).foreach { i =>
      val startTime = System.nanoTime()
      val compiled = H2Profile.queryCompiler.run(query.toNode, "H2Profile")
      val endTime = System.nanoTime()
      val timeMs = (endTime - startTime) / 1000000.0
      
      println(f"Compilation $i: $timeMs%.2f ms")
    }
    
    val finalStats = CompilationCache.getStats
    println()
    println(s"Final cache size: ${finalStats.size}")
    println(s"Cache hits: ${finalStats.hits}")
    println(s"Cache misses: ${finalStats.misses}")
    println(f"Hit rate: ${finalStats.hitRate * 100}%.1f%%")
    
    // Test with different queries
    println()
    println("Testing different queries...")
    
    val queries = Seq(
      users.filter(_.id === 2),
      users.filter(_.name === "test"),
      users.sortBy(_.id),
      users.take(10)
    )
    
    queries.zipWithIndex.foreach { case (q, i) =>
      val startTime = System.nanoTime()
      H2Profile.queryCompiler.run(q.toNode, "H2Profile")
      val endTime = System.nanoTime()
      val timeMs = (endTime - startTime) / 1000000.0
      
      println(f"Query ${i+1}: $timeMs%.2f ms")
    }
    
    val endStats = CompilationCache.getStats
    println()
    println(s"Final cache size: ${endStats.size}")
    println(s"Total hits: ${endStats.hits}")
    println(s"Total misses: ${endStats.misses}")
    println(f"Final hit rate: ${endStats.hitRate * 100}%.1f%%")
  }
}