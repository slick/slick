package slick.compiler

import scala.collection.concurrent.TrieMap
import scala.util.hashing.MurmurHash3
import slick.ast.Node
import org.slf4j.LoggerFactory
import com.typesafe.config.Config

/** A cache key based on AST structure and types for compilation results */
case class CacheKey(
  structureHash: Int,
  typeHash: Int,
  compilerPhases: Vector[String],
  profileName: String
) {
  override def hashCode(): Int = {
    var h = MurmurHash3.mix(structureHash, typeHash)
    h = MurmurHash3.mix(h, compilerPhases.hashCode())
    h = MurmurHash3.mix(h, profileName.hashCode())
    MurmurHash3.finalizeHash(h, 4)
  }
}

object CacheKey {
  def apply(node: Node, compiler: QueryCompiler, profileName: String): CacheKey = {
    val structureHash = computeStructureHash(node)
    val typeHash = computeTypeHash(node)
    val phaseNames = compiler.phases.map(_.name)
    CacheKey(structureHash, typeHash, phaseNames, profileName)
  }
  
  private def computeStructureHash(node: Node): Int = {
    def hashNode(n: Node): Int = {
      val childHashes = n.children.toSeq.map(hashNode)
      val nodeClassHash = n.getClass.getName.hashCode()
      
      // Extract structural information, avoiding object identity
      val dumpInfo = n.getDumpInfo
      val nodeStructuralHash = dumpInfo.name.hashCode()
      
      // Create a normalized representation that ignores symbol object identity
      val normalizedMainInfo = normalizeMainInfo(dumpInfo.mainInfo)
      val contentHash = normalizedMainInfo.hashCode()
      
      // Handle literal values specially by examining the node directly
      val literalHash = extractLiteralValue(n).hashCode()
      
      MurmurHash3.orderedHash(Seq(nodeClassHash, nodeStructuralHash, contentHash, literalHash) ++ childHashes, 0x3c5fbc5a)
    }
    hashNode(node)
  }
  
  private def normalizeMainInfo(mainInfo: String): String = {
    // Remove symbol object identities and other variable parts
    mainInfo
      .replaceAll("@[0-9a-fA-F]+", "@SYMBOL") // Remove object identity hashes
      .replaceAll("Symbol\\([^)]*\\)", "Symbol(NORMALIZED)") // Normalize symbol representations
      .replaceAll("\\$[0-9]+", "DOLLAR_N") // Normalize generated names (avoid $ in replacement)
  }
  
  private def extractLiteralValue(node: Node): String = {
    // Try to extract literal values using reflection since LiteralNode is package private
    try {
      val className = node.getClass.getSimpleName
      if (className.contains("LiteralNode") || className.contains("Literal")) {
        // Use toString as fallback for literal nodes, which should include the value
        val nodeStr = node.toString
        if (nodeStr.contains("LiteralNode(") || nodeStr.contains("value=")) {
          nodeStr
        } else {
          // Try to get value field via reflection
          val valueField = node.getClass.getDeclaredFields.find(_.getName == "value")
          valueField match {
            case Some(field) =>
              field.setAccessible(true)
              field.get(node).toString
            case None => ""
          }
        }
      } else {
        ""
      }
    } catch {
      case _: Exception => ""
    }
  }
  
  private def computeTypeHash(node: Node): Int = {
    def hashNodeType(n: Node): Int = {
      val childHashes = n.children.toSeq.map(hashNodeType)
      val typeHash = n.nodeType.hashCode()
      MurmurHash3.orderedHash(typeHash +: childHashes, 0x9e3f4d8b)
    }
    hashNodeType(node)
  }
}

/** Statistics for cache performance monitoring */
case class CacheStats(
  hits: Long = 0,
  misses: Long = 0,
  evictions: Long = 0,
  size: Int = 0
) {
  def hitRate: Double = if (hits + misses == 0) 0.0 else hits.toDouble / (hits + misses)
  def missRate: Double = 1.0 - hitRate
  
  def recordHit: CacheStats = copy(hits = hits + 1)
  def recordMiss: CacheStats = copy(misses = misses + 1)
  def recordEviction: CacheStats = copy(evictions = evictions + 1)
  def updateSize(newSize: Int): CacheStats = copy(size = newSize)
}

/** Cache configuration */
case class CacheConfig(
  enabled: Boolean = true,
  maxSize: Int = 1000,
  recordStats: Boolean = true,
  ttlSeconds: Long = 3600, // 1 hour
  logCacheOperations: Boolean = false
)

object CacheConfig {
  def fromConfig(config: Config): CacheConfig = {
    try {
      val cacheConfig = config.getConfig("slick.compiler.cache")
      
      CacheConfig(
        enabled = if (cacheConfig.hasPath("enabled")) cacheConfig.getBoolean("enabled") else true,
        maxSize = if (cacheConfig.hasPath("maxSize")) cacheConfig.getInt("maxSize") else 1000,
        recordStats = if (cacheConfig.hasPath("recordStats")) cacheConfig.getBoolean("recordStats") else true,
        ttlSeconds = if (cacheConfig.hasPath("ttlSeconds")) cacheConfig.getLong("ttlSeconds") else 3600,
        logCacheOperations = if (cacheConfig.hasPath("logCacheOperations")) cacheConfig.getBoolean("logCacheOperations") else false
      )
    } catch {
      case _: Exception => CacheConfig() // Return default config if configuration is missing
    }
  }
}

/** Cache entry with timestamp for TTL support */
case class CacheEntry(
  result: CompilerState,
  timestamp: Long = System.currentTimeMillis()
) {
  def isExpired(ttlMs: Long): Boolean = {
    System.currentTimeMillis() - timestamp > ttlMs
  }
}

/** Thread-safe compilation cache with LRU eviction and TTL support */
class CompilationCache(config: CacheConfig) {
  private val logger = LoggerFactory.getLogger(getClass)
  private val cache = new TrieMap[CacheKey, CacheEntry]()
  private val accessOrder = new TrieMap[CacheKey, Long]()
  private val accessCounter = new java.util.concurrent.atomic.AtomicLong(0)
  
  @volatile private var stats = new java.util.concurrent.atomic.AtomicReference(CacheStats())
  
  private val ttlMs = config.ttlSeconds * 1000
  
  def get(key: CacheKey): Option[CompilerState] = {
    if (!config.enabled) return None
    
    cache.get(key) match {
      case Some(entry) if !entry.isExpired(ttlMs) =>
        // Update access order for LRU
        accessOrder.put(key, accessCounter.incrementAndGet())
        if (config.recordStats) {
          stats.updateAndGet(_.recordHit)
        }
        
        if (config.logCacheOperations) {
          logger.debug(s"Cache hit for key: ${key.structureHash}")
        }
        Some(entry.result)
        
      case Some(entry) =>
        // Entry expired, remove it
        cache.remove(key)
        accessOrder.remove(key)
        if (config.recordStats) {
          stats.updateAndGet(_.recordMiss.recordEviction.updateSize(cache.size))
        }
        
        if (config.logCacheOperations) {
          logger.debug(s"Cache miss (expired) for key: ${key.structureHash}")
        }
        None
        
      case None =>
        if (config.recordStats) {
          stats.updateAndGet(_.recordMiss)
        }
        
        if (config.logCacheOperations) {
          logger.debug(s"Cache miss for key: ${key.structureHash}")
        }
        None
    }
  }
  
  def put(key: CacheKey, result: CompilerState): Unit = {
    if (!config.enabled) return
    
    // Check if we need to evict entries
    if (cache.size >= config.maxSize) {
      evictLRU()
    }
    
    val entry = CacheEntry(result)
    cache.put(key, entry)
    accessOrder.put(key, accessCounter.incrementAndGet())
    if (config.recordStats) {
      stats.updateAndGet(_.updateSize(cache.size))
    }
    
    if (config.logCacheOperations) {
      logger.debug(s"Cache put for key: ${key.structureHash}")
    }
  }
  
  private def evictLRU(): Unit = {
    // Find the least recently used entry
    val lruKey = if (accessOrder.nonEmpty) {
      Some(accessOrder.minBy(_._2)._1)
    } else {
      None
    }
    
    lruKey.foreach { key =>
      cache.remove(key)
      accessOrder.remove(key)
      if (config.recordStats) {
        stats.updateAndGet(_.recordEviction.updateSize(cache.size))
      }
      
      if (config.logCacheOperations) {
        logger.debug(s"Evicted LRU entry for key: ${key.structureHash}")
      }
    }
  }
  
  def clear(): Unit = {
    cache.clear()
    accessOrder.clear()
    stats.set(CacheStats())
  }
  
  def size: Int = cache.size
  
  def getStats: CacheStats = {
    if (config.recordStats) {
      stats.updateAndGet(_.updateSize(cache.size))
    } else {
      CacheStats().updateSize(cache.size)
    }
  }
  
  def cleanupExpired(): Unit = {
    if (!config.enabled) return
    
    val now = System.currentTimeMillis()
    val expiredKeys = cache.filter { case (_, entry) => entry.isExpired(ttlMs) }.keys
    
    expiredKeys.foreach { key =>
      cache.remove(key)
      accessOrder.remove(key)
      if (config.recordStats) {
        stats.updateAndGet(_.recordEviction)
      }
    }
    
    if (config.recordStats) {
      stats.updateAndGet(_.updateSize(cache.size))
    }
    
    if (config.logCacheOperations && expiredKeys.nonEmpty) {
      logger.debug(s"Cleaned up ${expiredKeys.size} expired cache entries")
    }
  }
}

/** Global compilation cache instance */
object CompilationCache {
  private val logger = LoggerFactory.getLogger(getClass)
  
  private lazy val config = try {
    import com.typesafe.config.ConfigFactory
    val baseConfig = CacheConfig.fromConfig(ConfigFactory.load())
    
    // Disable cache during testing if system property is set
    val isTestRun = sys.props.get("slick.testRun").exists(_.toLowerCase == "true")
    if (isTestRun) {
      baseConfig.copy(enabled = false)
    } else {
      baseConfig
    }
  } catch {
    case _: Exception => CacheConfig() // Use default config if none available
  }
  private lazy val globalCache = new CompilationCache(config)
  
  // Cleanup thread for expired entries
  private val cleanupThread: Option[Thread] = if (config.enabled) {
    val thread = new Thread(new Runnable {
      def run(): Unit = {
        while (true) {
          try {
            Thread.sleep(config.ttlSeconds * 1000 / 4) // Cleanup every quarter of TTL
            globalCache.cleanupExpired()
          } catch {
            case _: InterruptedException => 
              Thread.currentThread().interrupt()
              return
            case ex: Exception => logger.warn("Error during cache cleanup", ex)
          }
        }
      }
    })
    thread.setDaemon(true)
    thread.setName("slick-compilation-cache-cleanup")
    thread.start()
    Some(thread)
  } else {
    None
  }
  
  def get(key: CacheKey): Option[CompilerState] = globalCache.get(key)
  def put(key: CacheKey, result: CompilerState): Unit = globalCache.put(key, result)
  def clear(): Unit = globalCache.clear()
  def size: Int = globalCache.size
  def getStats: CacheStats = globalCache.getStats
  def isEnabled: Boolean = config.enabled
  
  // JVM shutdown hook to clean up
  Runtime.getRuntime.addShutdownHook(new Thread(() => {
    cleanupThread.foreach(_.interrupt())
  }))
}