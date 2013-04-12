package scala.slick.memory

import scala.slick.backend.DatabaseComponent
import scala.slick.SlickException
import scala.slick.ast.{ColumnOption, FieldSymbol}
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.slick.util.Logging
import java.util.concurrent.atomic.AtomicLong

/** A simple database engine that stores data in heap data structures. */
trait HeapBackend extends DatabaseComponent with Logging {

  type Database = DatabaseDef
  type Session = SessionDef
  type DatabaseFactory = DatabaseFactoryDef

  val Database = new DatabaseFactoryDef
  val backend: HeapBackend = this

  class DatabaseDef extends super.DatabaseDef {
    protected val tables = new HashMap[String, HeapTable]
    def createSession(): Session = new SessionDef(this)
    def getTable(name: String): HeapTable = synchronized {
      tables.get(name).getOrElse(throw new SlickException(s"Table $name does not exist"))
    }
    def createTable(name: String, columns: IndexedSeq[HeapBackend.Column]): HeapTable = synchronized {
      if(tables.contains(name)) throw new SlickException(s"Table $name already exists")
      val t = new HeapTable(name, columns)
      tables += ((name, t))
      t
    }
    def dropTable(name: String): Unit = synchronized {
      if(!tables.remove(name).isDefined)
        throw new SlickException(s"Table $name does not exist")
    }
    def getTables: IndexedSeq[HeapTable] = synchronized {
      tables.values.toVector
    }
  }

  class DatabaseFactoryDef extends super.DatabaseFactoryDef {
    def apply(): Database = new DatabaseDef
  }

  class SessionDef(val database: Database) extends super.SessionDef {
    def close() {}

    def rollback() =
      throw new SlickException("HeapBackend does not currently support transactions")

    def force() {}

    def withTransaction[T](f: => T) =
      throw new SlickException("HeapBackend does not currently support transactions")
  }

  type Row = IndexedSeq[Any]

  class HeapTable(val name: String, val columns: IndexedSeq[HeapBackend.Column]) {
    protected val data: ArrayBuffer[Row] = new ArrayBuffer[Row]
    def rows: Iterable[Row] = data
    def append(row: Row): Unit = {
      data.append(row)
      logger.debug("Inserted ("+row.mkString(", ")+") into "+this)
    }
    def createInsertRow: ArrayBuffer[Any] = columns.map(_.createDefault)(collection.breakOut)
    override def toString = name + "(" + columns.map(_.sym.name).mkString(", ") + ")"
  }
}

object HeapBackend extends HeapBackend {
  class Column(val sym: FieldSymbol, val tpe: ScalaType[Any]) {
    private[this] val default = sym.options.collectFirst { case ColumnOption.Default(v) => v }
    private[this] val autoInc = sym.options.collectFirst { case ColumnOption.AutoInc => new AtomicLong() }
    def createDefault: Any = autoInc match {
      case Some(a) =>
        val i = a.incrementAndGet()
        if(tpe == ScalaType.longType) i
        else if(tpe == ScalaType.intType) i.toInt
        else throw new SlickException("Only Long and Int types are allowed for AutoInc columns")
      case None => default.getOrElse(tpe.zero)
    }
  }
}
