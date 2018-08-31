package slick.memory

import java.util.concurrent.atomic.AtomicLong

import com.typesafe.config.Config

import org.reactivestreams.Subscriber

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}
import scala.concurrent.{Future, ExecutionContext}

import slick.SlickException
import slick.ast._
import slick.lifted.{PrimaryKey, Constraint, Index}
import slick.relational.{RelationalProfile, RelationalBackend}
import slick.util.Logging

/** A simple database engine that stores data in heap data structures. */
trait HeapBackend extends RelationalBackend with Logging {
  type This = HeapBackend
  type Database = DatabaseDef
  type Session = SessionDef
  type DatabaseFactory = DatabaseFactoryDef
  type Context = BasicActionContext
  type StreamingContext = BasicStreamingActionContext

  val Database = new DatabaseFactoryDef
  val backend: HeapBackend = this

  def createDatabase(config: Config, path: String): Database = Database.apply(ExecutionContext.global)

  class DatabaseDef(protected val synchronousExecutionContext: ExecutionContext) extends super.DatabaseDef {
    protected[this] def createDatabaseActionContext[T](_useSameThread: Boolean): Context =
      new BasicActionContext { val useSameThread = _useSameThread }

    protected[this] def createStreamingDatabaseActionContext[T](s: Subscriber[_ >: T], useSameThread: Boolean): StreamingContext =
      new BasicStreamingActionContext(s, useSameThread, DatabaseDef.this)

    protected val tables = new HashMap[String, HeapTable]
    def createSession(): Session = new SessionDef(this)
    override def shutdown: Future[Unit] = Future.successful(())
    def close: Unit = ()
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
    def createTableIfNotExists(name: String, columns: IndexedSeq[HeapBackend.Column],
                    indexes: IndexedSeq[Index], constraints: IndexedSeq[Constraint]): HeapTable = synchronized {
      val t = new HeapTable(name, columns, indexes, constraints)
      if(!tables.contains(name)) tables += ((name, t))
      t
    }
    def dropTable(name: String): Unit = synchronized {
      if(!tables.remove(name).isDefined)
        throw new SlickException(s"Table $name does not exist")
    }
    def dropTableIfExists(name: String): Unit = try dropTable(name) catch{
      case e: SlickException => ()
      case e: Throwable => throw e
    }
    def truncateTable(name: String): Unit = synchronized{
      getTable(name).data.clear
    }
    def getTables: IndexedSeq[HeapTable] = synchronized {
      tables.values.toVector
    }
  }

  def createEmptyDatabase: Database = {
    def err = throw new SlickException("Unsupported operation for empty heap database")
    new DatabaseDef(new ExecutionContext {
      def reportFailure(t: Throwable) = err
      def execute(runnable: Runnable) = err
    }) {
      override def createTable(name: String, columns: IndexedSeq[HeapBackend.Column],
                               indexes: IndexedSeq[Index], constraints: IndexedSeq[Constraint]) = err
    }
  }

  class DatabaseFactoryDef {
    /** Create a new heap database instance that uses the supplied ExecutionContext for
      * asynchronous execution of database actions. */
    def apply(executionContext: ExecutionContext): Database = new DatabaseDef(executionContext)
  }

  class SessionDef(val database: Database) extends super.SessionDef {
    def close(): Unit = {}

    def rollback() =
      throw new SlickException("HeapBackend does not currently support transactions")

    def force(): Unit = {}

    def withTransaction[T](f: => T) =
      throw new SlickException("HeapBackend does not currently support transactions")
  }

  type Row = IndexedSeq[Any]

  class HeapTable(val name: String, val columns: IndexedSeq[HeapBackend.Column],
                  indexes: IndexedSeq[Index], constraints: IndexedSeq[Constraint]) {
    protected[HeapBackend] val data: ArrayBuffer[Row] = new ArrayBuffer[Row]

    def rows: Iterable[Row] = data

    def append(row: Row): Unit = synchronized {
      verifier.verify(row)
      data.append(row)
      verifier.inserted(row)
      logger.debug("Inserted ("+row.mkString(", ")+") into "+this)
    }

    def createInsertRow: ArrayBuffer[Any] = columns.map(_.createDefault)(collection.breakOut)

    override def toString = name + "(" + columns.map(_.sym.name).mkString(", ") + ")"

    lazy val columnIndexes: Map[TermSymbol, Int] = columns.map(_.sym).zipWithIndex.toMap

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
        def verify(row: Row): Unit = {
          val e = extract(row)
          if(hash contains e)
            throw new SlickException("Uniqueness constraint "+name+" violated. Duplicate data: "+e)
        }
        def inserted(row: Row): Unit = { hash += extract(row) }
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
        def verify(row: Row): Unit = { self.verify(row); other.verify(row) }
        def inserted(row: Row): Unit = { self.inserted(row); other.inserted(row) }
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
    private[this] val default = sym.options.collectFirst { case RelationalProfile.ColumnOption.Default(v) => v }
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
