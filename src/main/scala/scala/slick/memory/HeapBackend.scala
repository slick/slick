package scala.slick.memory

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}
import scala.slick.SlickException
import scala.slick.ast._
import scala.slick.backend.DatabaseComponent
import scala.slick.lifted.{PrimaryKey, Constraint, Index}
import scala.slick.util.Logging

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
    def createTable(name: String, columns: IndexedSeq[HeapBackend.Column],
                    indexes: IndexedSeq[Index], constraints: IndexedSeq[Constraint]): HeapTable = synchronized {
      if(tables.contains(name)) throw new SlickException(s"Table $name already exists")
      val t = new HeapTable(name, columns, indexes, constraints)
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

  def createEmptyDatabase: Database = new DatabaseDef {
    override def createTable(name: String, columns: IndexedSeq[HeapBackend.Column],
                    indexes: IndexedSeq[Index], constraints: IndexedSeq[Constraint]) =
      throw new SlickException("Cannot modify empty heap database")
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

  class HeapTable(val name: String, val columns: IndexedSeq[HeapBackend.Column],
                  indexes: IndexedSeq[Index], constraints: IndexedSeq[Constraint]) {
    protected val data: ArrayBuffer[Row] = new ArrayBuffer[Row]

    def rows: Iterable[Row] = data

    def append(row: Row): Unit = synchronized {
      verifier.verify(row)
      data.append(row)
      verifier.inserted(row)
      logger.debug("Inserted ("+row.mkString(", ")+") into "+this)
    }

    def createInsertRow: ArrayBuffer[Any] = columns.map(_.createDefault)(collection.breakOut)

    override def toString = name + "(" + columns.map(_.sym.name).mkString(", ") + ")"

    lazy val columnIndexes: Map[Symbol, Int] = columns.map(_.sym).zipWithIndex.toMap

    val verifier = {
      val v1 = indexes.foldLeft(Verifier.empty) { case (z, i) => z andThen createIndexVerifier(i) }
      val v2 = constraints.foldLeft(v1) { case (z, c) => z andThen createConstraintVerifier(c) }
      columns.foldLeft(v2) { case (z, c) =>
        if(c.isUnique) z andThen createUniquenessVerifier("<unique column "+c.sym.name+">", Vector(c.sym))
        else z
      }
    }

    protected def createConstraintVerifier(cons: Constraint) = cons match {
      case PrimaryKey(name, columns) => createUniquenessVerifier(name, columns.map { case Select(_, f: FieldSymbol) => f })
      case _ => Verifier.empty
    }

    protected def createIndexVerifier(idx: Index) =
      if(!idx.unique) Verifier.empty
      else createUniquenessVerifier(idx.name, idx.on.map { case Select(_, f: FieldSymbol) => f })

    protected def createUniquenessVerifier(name: String, on: IndexedSeq[FieldSymbol]): Verifier = {
      val columns: IndexedSeq[Int] = on.map(columnIndexes)
      val extract: (Row => Any) =
        if(columns.length == 1) (r: Row) => r(columns.head)
        else (r: Row) => columns.map(r)
      val hash = new HashSet[Any]()
      new Verifier {
        def verify(row: Row) {
          val e = extract(row)
          if(hash contains e)
            throw new SlickException("Uniqueness constraint "+name+" violated. Duplicate data: "+e)
        }
        def inserted(row: Row) { hash += extract(row) }
      }
    }
  }

  /** A Verifier is called before and after data is updated in a table. It
    * ensures that no constraints are violated before the update and updates
    * the indices afterwards. */
  trait Verifier { self =>
    def verify(row: Row): Unit
    def inserted(row: Row): Unit

    def andThen(other: Verifier): Verifier =
      if(this eq Verifier.empty) other
      else if(other eq Verifier.empty) this
      else new Verifier {
        def verify(row: Row) { self.verify(row); other.verify(row) }
        def inserted(row: Row) { self.inserted(row); other.inserted(row) }
      }
  }

  object Verifier {
    val empty: Verifier = new Verifier {
      def verify(row: Row) = ()
      def inserted(row: Row) = ()
    }
  }
}

object HeapBackend extends HeapBackend {
  class Column(val sym: FieldSymbol, val tpe: ScalaType[Any]) {
    private[this] val default = sym.options.collectFirst { case ColumnOption.Default(v) => v }
    private[this] val autoInc = sym.options.collectFirst { case ColumnOption.AutoInc => new AtomicLong() }
    val isUnique =  sym.options.collectFirst { case ColumnOption.PrimaryKey => true }.getOrElse(false)
    def createDefault: Any = autoInc match {
      case Some(a) =>
        val i = a.incrementAndGet()
        if(tpe == ScalaBaseType.longType) i
        else if(tpe == ScalaBaseType.intType) i.toInt
        else throw new SlickException("Only Long and Int types are allowed for AutoInc columns")
      case None => default.getOrElse(null)
    }
  }
}
