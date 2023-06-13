package slick.util

import java.beans.Introspector
import java.util.Properties

import scala.jdk.CollectionConverters.*

import slick.SlickException

import com.typesafe.config.ConfigFactory

/** Configure Java Beans reflectively, using Typesafe Config for data type conversions. */
object BeanConfigurator extends Logging {
  def configure(o: AnyRef, p: Properties, allowed: Set[String] = Set.empty): Unit = {
    val pds = Introspector.getBeanInfo(o.getClass).getPropertyDescriptors.iterator.map(pd => (pd.getName, pd)).toMap
    p.propertyNames().asScala.foreach { key =>
      val name = key.toString
      if(allowed.isEmpty || allowed.contains(name)) {
        val v = Option(p.getProperty(name)).getOrElse(p.get(key))
        pds.get(name) match {
          case Some(pd) =>
            try {
              val tp = pd.getPropertyType
              if(tp == classOf[Int]) pd.getWriteMethod.invoke(o, toInt(v))
              else if(tp == classOf[Long]) pd.getWriteMethod.invoke(o, toLong(v))
              else if(tp == classOf[Boolean]) pd.getWriteMethod.invoke(o, toBoolean(v))
              else if(tp == classOf[String]) pd.getWriteMethod.invoke(o, v.toString)
              else pd.getWriteMethod.invoke(o, v)
            } catch { case ex: Exception =>
              throw new SlickException(s"Error setting bean property $name on target ${o.getClass}", ex)
            }
          case None =>
            logger.warn(s"Ignoring unsupported bean property $name on target ${o.getClass}")
        }
      }
    }
  }

  private def toInt(o: Any): java.lang.Integer = ConfigFactory.parseMap(Map("v" -> o.toString).asJava).getInt("v")
  private def toLong(o: Any): java.lang.Long = ConfigFactory.parseMap(Map("v" -> o.toString).asJava).getLong("v")
  private def toBoolean(o: Any): java.lang.Boolean = ConfigFactory.parseMap(Map("v" -> o.toString).asJava).getBoolean("v")
}
