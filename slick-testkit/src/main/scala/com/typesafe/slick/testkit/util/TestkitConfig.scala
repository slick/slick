package com.typesafe.slick.testkit.util

import java.io.File
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

import slick.SlickException

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}


/** Manages the configuration for TestKit tests.
  *
  * The standard config file `test-dbs/testkit.conf` can be changed through the system
  * property `slick.testkit-config`. The defaults are loaded from `/testkit-reference.conf`
  * on the classpath.
  */
object TestkitConfig {
  private[this] lazy val (conf, testkitConfig, defaults, ref) = {
    val configFileName = sys.props.get("slick.testkit-config").orElse(sys.env.get("SLICK_TESTKIT_CONFIG"))
    val configFile = new File(configFileName.getOrElse("test-dbs/testkit.conf"))
    if(configFileName.isDefined && !configFile.isFile)
      throw new SlickException("TestKit config file \""+configFileName.get+"\" not found")
    val ref = ConfigFactory.parseResources(getClass, "/testkit-reference.conf")
    val conf = ConfigFactory.systemProperties().withFallback(ConfigFactory.parseFile(configFile))
    val testkitConfig = {
      val c =
        if(conf.hasPath("testkit")) conf.getConfig("testkit").withFallback(ref.getObject("testkit")).resolve()
        else ref.getConfig("testkit").resolve()
      c.withValue("absTestDir", ConfigValueFactory.fromAnyRef(new File(c.getString("testDir")).getAbsolutePath))
    }
    val defaults = ref.getObject("defaults").withValue("testkit", testkitConfig.root()).toConfig
    (conf, testkitConfig, defaults, ref)
  }

  /** Get a resolved test configuration */
  def testConfig(name: String) = {
    val cRef = if(ref.hasPath(name)) ref.getConfig(name).withFallback(defaults) else defaults
    val cApp = if(conf.hasPath(name)) conf.getConfig(name).withFallback(cRef) else cRef
    cApp.resolve().withoutPath("testkit")
  }

  /** The `testkit.testDir` setting */
  lazy val testDir = testkitConfig.getString("testDir")

  /** A normalized version of `testDir` for use in URLs */
  lazy val testDBPath = {
    val f = new File(testDir)
    val s = f.getPath.replace('\\', '/')
    if(f.isAbsolute) s else "./" + s
  }

  /** The `testkit.testDBs` setting */
  lazy val testDBs = getStrings(testkitConfig, "testDBs")

  /** The `testkit.testClasses` setting */
  lazy val testClasses: Seq[Class[_ <: AsyncTest[_ >: Null <: TestDB]]] =
    getStrings(testkitConfig, "testClasses").getOrElse(Nil).
      map(n => Class.forName(n).asInstanceOf[Class[_ <: AsyncTest[_ >: Null <: TestDB]]])

  /** The duration after which asynchronous tests should be aborted and failed */
  lazy val asyncTimeout =
    Duration(testkitConfig.getDuration("asyncTimeout", TimeUnit.MILLISECONDS), TimeUnit.MILLISECONDS)

  def getStrings(config: Config, path: String): Option[Seq[String]] = {
    if(config.hasPath(path)) {
      config.getValue(path).unwrapped() match {
        case l: java.util.List[_] => Some(l.asScala.iterator.map(_.toString).toSeq)
        case o => Some(List(o.toString))
      }
    } else None
  }
}
