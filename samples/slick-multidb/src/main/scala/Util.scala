import java.sql.DriverManager
import scala.collection.JavaConverters._

object Util {
  /** A helper function to unload all JDBC drivers so we don't leak memory */
  def unloadDrivers {
    DriverManager.getDrivers.asScala.foreach { d =>
      DriverManager.deregisterDriver(d)
    }
  }
}
