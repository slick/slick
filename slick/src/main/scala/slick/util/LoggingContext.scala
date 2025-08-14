package slick.util

/** Context information for logging that can be propagated through database operations.
  * 
  * @param values Key-value pairs that will be included in log messages
  */
case class LoggingContext(values: Map[String, String]) {
  
  /** Add a key-value pair to this context */
  def +(kv: (String, String)): LoggingContext = 
    LoggingContext(values + kv)
  
  /** Add multiple key-value pairs to this context */
  def ++(other: Map[String, String]): LoggingContext = 
    LoggingContext(values ++ other)
  
  /** Merge this context with another, with other taking precedence for duplicate keys */
  def merge(other: LoggingContext): LoggingContext = 
    LoggingContext(values ++ other.values)
  
  /** Format context as a string for inclusion in log messages */
  def formatForLogging: String = {
    if (values.isEmpty) ""
    else values.map { case (k, v) => s"$k=$v" }.mkString("[", ", ", "] ")
  }
  
  /** Check if this context is empty */
  def isEmpty: Boolean = values.isEmpty
  
  /** Check if this context is non-empty */
  def nonEmpty: Boolean = values.nonEmpty
}

object LoggingContext {
  
  /** Empty logging context */
  val empty: LoggingContext = LoggingContext(Map.empty[String, String])
  
  /** Create a context from key-value pairs */
  def apply(kvs: (String, String)*): LoggingContext = 
    LoggingContext(kvs.toMap)
  
  /** Create a context from a single key-value pair */
  def single(key: String, value: String): LoggingContext = 
    LoggingContext(Map(key -> value))
}