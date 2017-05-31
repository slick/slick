package slick.compiler

import scala.collection.immutable.HashMap
import slick.SlickException
import slick.util._
import slick.ast.{SymbolNamer, Node}
import org.slf4j.LoggerFactory

/** An immutable, stateless query compiler consisting of a series of phases */
class QueryCompiler(val phases: Vector[Phase]) extends Logging {
  protected[this] lazy val benchmarkLogger = new SlickLogger(LoggerFactory.getLogger(getClass.getName+"Benchmark"))

  /** Return a new compiler with the new phase added at the end. */
  def + (p: Phase) = new QueryCompiler(phases :+ p)

  /** Return a new compiler with the new phases added at the end. */
  def ++ (ps: Seq[Phase]) = new QueryCompiler(phases ++ ps)

  /** Return a new compiler with the new phase added directly after another
   * phase (or a different implementation of the same phase name). */
  def addAfter(p: Phase, after: Phase) = new QueryCompiler({
    val i = phases.lastIndexWhere(_.name == after.name)
    if(i == -1) throw new SlickException("Previous phase "+after.name+" not found")
    else phases.patch(i+1, Seq(p), 0)
  })

  /** Return a new compiler with the new phase added directly before another
    * phase (or a different implementation of the same phase name). */
  def addBefore(p: Phase, before: Phase) = new QueryCompiler({
    val i = phases.indexWhere(_.name == before.name)
    if(i == -1) throw new SlickException("Following phase "+before.name+" not found")
    else phases.patch(i, Seq(p), 0)
  })

  /** Return a new compiler without the given phase (or a different
   * implementation of the same phase name. */
  def - (p: Phase) = new QueryCompiler(phases.filterNot(_.name == p.name))

  /** Return a new compiler that replaces an existing phase by a new one with
   * the same name. The new phase must have a State that is assignable to the
   * original phase's state. */
  def replace(p: Phase) = new QueryCompiler(phases.map(o => if(o.name == p.name) p else o))

  /** Compile an AST with a new `CompilerState`. */
  def run(tree: Node): CompilerState = {
    val state = new CompilerState(this, tree)
    run(state)
  }

  /** Compile an AST in an existing `CompilerState`. This can be used for triggering
    * compilation of subtrees within the current `CompilerState`. */
  def run(state: CompilerState): CompilerState =
    runPhases(phases.iterator, state)

  /** Compile an AST in an existing `CompilerState`, stopping just before the specified phase.
    * This can be used for triggering compilation of subtrees within the current `CompilerState`. */
  def runBefore(before: Phase, state: CompilerState): CompilerState =
    runPhases(phases.iterator.takeWhile(_.name != before.name), state)

  protected[this] def runPhases(it: Iterator[Phase], state: CompilerState): CompilerState = {
    if(logger.isDebugEnabled) state.symbolNamer.use { logger.debug("Source:", state.tree) }
    if(benchmarkLogger.isDebugEnabled) {
      val (res, times) = it.foldLeft((state, Nil: List[(String, Long)])){ case ((n, times), p) =>
        val t0 = System.nanoTime()
        val pout = runPhase(p, n)
        val time = System.nanoTime() - t0
        (pout, (p.name, time) :: times)
      }
      benchmarkLogger.debug("------------------- Phase: Time ---------")
      (("TOTAL", times.map(_._2).sum) :: times).reverse.foreach { case (name, nanos) =>
        val millis = nanos / 1000000.0
        benchmarkLogger.debug(f"$name%25s: $millis%11.6f ms")
      }
      res
    } else it.foldLeft(state){ case (n,p) => runPhase(p, n) }
  }

  protected[this] def runPhase(p: Phase, state: CompilerState): CompilerState = state.symbolNamer.use {
    val s2 = p(state)
    if(s2.tree ne state.tree) {
      if(logger.isDebugEnabled) {
        if(GlobalConfig.detectRebuild && s2.tree == state.tree) {
          val rebuilt = detectRebuiltLeafs(state.tree, s2.tree)
          logger.debug("After phase "+p.name+": (no change but not identical)", s2.tree, (d => rebuilt.contains(RefId(d))))
        } else
          logger.debug("After phase "+p.name+":", s2.tree)
      }
      if(GlobalConfig.verifyTypes && s2.wellTyped)
        (new VerifyTypes(after = Some(p))).apply(s2)
    }
    else logger.debug("After phase "+p.name+": (no change)")
    s2
  }

  protected[this] def detectRebuiltLeafs(n1: Node, n2: Node): Set[RefId[Dumpable]] = {
    if(n1 eq n2) Set.empty else {
      val chres =
        n1.children.iterator.zip(n2.children.iterator).map { case (n1, n2) => detectRebuiltLeafs(n1, n2) }.foldLeft(Set.empty[RefId[Dumpable]])(_ ++ _)
      if(chres.isEmpty) Set(RefId(n2)) else chres
    }
  }
}

object QueryCompiler {
  /** The standard phases of the query compiler */
  val standardPhases = Vector(
    /* Clean up trees from the lifted embedding */
    Phase.assignUniqueSymbols,
    Phase.unrollTailBinds,
    /* Distribute and normalize */
    Phase.inferTypes,
    Phase.expandTables,
    Phase.forceOuterBinds,
    Phase.removeMappedTypes,
    /* Convert to column form */
    Phase.expandSums,
    // optional removeTakeDrop goes here
    // optional emulateOuterJoins goes here
    Phase.expandRecords,
    Phase.flattenProjections,
    /* Optimize for SQL */
    Phase.rewriteJoins,
    Phase.verifySymbols,
    Phase.relabelUnions
  )

  /** Extra phases for translation to SQL comprehensions */
  val sqlPhases = Vector(
    // optional access:existsToCount goes here
    Phase.createAggregates,
    Phase.resolveZipJoins,
    Phase.pruneProjections,
    Phase.rewriteDistinct,
    Phase.createResultSetMapping,
    Phase.hoistClientOps,
    Phase.reorderOperations,
    Phase.mergeToComprehensions,
    Phase.optimizeScalar,
    Phase.fixRowNumberOrdering,
    Phase.removeFieldNames
    // optional rewriteBooleans goes here
    // optional specializeParameters goes here
  )

  /** Extra phases needed for the QueryInterpreter */
  val interpreterPhases = Vector(
    Phase.pruneProjections,
    Phase.createResultSetMapping,
    Phase.removeFieldNames
  )

  /** The default compiler */
  val standard = new QueryCompiler(standardPhases)

  /** Construct a new `QueryCompiler` with the given phases */
  def apply(phases: Phase*) = new QueryCompiler(phases.toVector)
}

/** A phase of the query compiler, identified by a unique name */
trait Phase extends (CompilerState => CompilerState) with Logging {
  /** The immutable state of the phase that can also be accessed by other phases. */
  type State

  /** The unique name of the phase */
  val name: String

  /** Run the phase */
  def apply(state: CompilerState): CompilerState
}

/** The `Phase` companion objects contains ready-to-use `Phase` objects for
  * the standard phases of the query compiler */
object Phase {
  /* The standard phases of the query compiler */
  val unrollTailBinds = new UnrollTailBinds
  val assignUniqueSymbols = new AssignUniqueSymbols
  val inferTypes = new InferTypes
  val expandTables = new ExpandTables
  val forceOuterBinds = new ForceOuterBinds
  val removeMappedTypes = new RemoveMappedTypes
  val expandSums = new ExpandSums
  val expandRecords = new ExpandRecords
  val flattenProjections = new FlattenProjections
  val createAggregates = new CreateAggregates
  val rewriteJoins = new RewriteJoins
  val verifySymbols = new VerifySymbols
  val resolveZipJoins = new ResolveZipJoins
  val createResultSetMapping = new CreateResultSetMapping
  val hoistClientOps = new HoistClientOps
  val reorderOperations = new ReorderOperations
  val relabelUnions = new RelabelUnions
  val mergeToComprehensions = new MergeToComprehensions
  val optimizeScalar = new OptimizeScalar
  val fixRowNumberOrdering = new FixRowNumberOrdering
  val pruneProjections = new PruneProjections
  val rewriteDistinct = new RewriteDistinct
  val removeFieldNames = new RemoveFieldNames

  /* Extra phases that are not enabled by default */
  val removeTakeDrop = new RemoveTakeDrop
  val resolveZipJoinsRownumStyle = new ResolveZipJoins(rownumStyle = true)
  val rewriteBooleans = new RewriteBooleans
  val specializeParameters = new SpecializeParameters
}

/** The current state of a compiler run, consisting of the current AST and
  * additional immutable state of individual phases. Mutability is confined
  * to the SymbolNamer. The state is tied to a specific compiler instance so
  * that phases can call back into the compiler. */
class CompilerState private (val compiler: QueryCompiler, val symbolNamer: SymbolNamer,
                             val tree: Node, state: HashMap[String, Any], val wellTyped: Boolean) {
  def this(compiler: QueryCompiler, tree: Node) =
    this(compiler, new SymbolNamer("s", "t"), tree, new HashMap, false)

  /** Get the phase state for a phase */
  def get[P <: Phase](p: P): Option[p.State] = state.get(p.name).asInstanceOf[Option[p.State]]

  /** Return a new `CompilerState` with the given mapping of phase to phase state */
  def + [S, P <: Phase { type State = S }](t: (P, S)) =
    new CompilerState(compiler, symbolNamer, tree, state + (t._1.name -> t._2), wellTyped)

  /** Return a new `CompilerState` which encapsulates the specified AST */
  def withNode(tree: Node) = new CompilerState(compiler, symbolNamer, tree, state, wellTyped)

  /** Return a new `CompilerState` which indicates whether or not the AST should be well-typed
    * after every phase. */
  def withWellTyped(wellTyped: Boolean) = new CompilerState(compiler, symbolNamer, tree, state, wellTyped)

  /** Return a new `CompilerState` with a transformed AST */
  def map(f: Node => Node) = withNode(f(tree))
}
