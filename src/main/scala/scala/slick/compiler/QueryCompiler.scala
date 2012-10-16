package scala.slick.compiler

import scala.collection.mutable.HashMap
import scala.slick.SlickException
import scala.slick.util.Logging
import scala.slick.ast.{SymbolNamer, Node}

/** An immutable, stateless query compiler consisting of a series of phases */
class QueryCompiler(val phases: Vector[Phase]) extends Logging {

  /** Return a new compiler with the new phase added at the end. */
  def + (p: Phase) = new QueryCompiler(phases :+ p)

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

  def run(tree: Node): CompilationState = {
    val state = new CompilationState(this)
    state.ast = tree
    run(tree, state)
    state
  }

  def run(tree: Node, state: CompilationState): Node = {
    logger.debug("Source:", tree)
    phases.foldLeft(tree){ case (n,p) => runPhase(p, n, state) }
  }

  def runBefore(before: Phase, tree: Node, state: CompilationState): Node = {
    logger.debug("Source:", tree)
    phases.iterator.takeWhile(_.name != before.name).foldLeft(tree){ case (n,p) => runPhase(p, n, state) }
  }

  protected[this] def runPhase(p: Phase, tree: Node, state: CompilationState): Node = state.symbolNamer.use {
    state.ast = tree
    val t2 = p(tree, state)
    if(t2 ne tree) {
      state.ast = t2
      logger.debug("After phase "+p.name+":", t2)
    } else logger.debug("After phase "+p.name+": (no change)")
    t2
  }
}

object QueryCompiler {
  val standardPhases = Vector(
    // Optimizer
    Phase.localizeRefs,
    Phase.reconstructProducts,
    Phase.inline,
    Phase.rewriteOrderBy,
    Phase.letDynamicEliminated,
    Phase.assignUniqueSymbols,
    // Columnizer
    Phase.forceOuterBinds,
    Phase.expandTables,
    Phase.expandRefs,
    Phase.replaceFieldSymbols,
    // PathRewriter
    Phase.rewritePaths,
    Phase.relabelUnions,
    Phase.pruneFields
  )

  val relationalPhases = Vector(
    Phase.resolveZipJoins,
    Phase.convertToComprehensions,
    Phase.fuseComprehensions,
    Phase.fixRowNumberOrdering
  )

  /** The default compiler */
  val standard = new QueryCompiler(standardPhases)

  /** The default compiler with the additional conversion to relational trees */
  val relational = new QueryCompiler(standardPhases ++ relationalPhases)
}

/** A phase of the query compiler, identified by a unique name */
trait Phase extends ((Node, CompilationState) => Node) with Logging {
  /** The immutable state of the phase that can also be accessed by other phases. */
  type State

  /** The unique name of the phase */
  val name: String

  /** Run the phase */
  def apply(tree: Node, state: CompilationState): Node
}

/** The standard phases of the query compiler */
object Phase {
  val localizeRefs = new LocalizeRefs
  val reconstructProducts = new ReconstructProducts
  val inline = new Inline
  val rewriteOrderBy = new RewriteOrderBy
  val letDynamicEliminated = new LetDynamicEliminated
  val assignUniqueSymbols = new AssignUniqueSymbols
  val forceOuterBinds = new ForceOuterBinds
  val expandTables = new ExpandTables
  val expandRefs = new ExpandRefs
  val replaceFieldSymbols = new ReplaceFieldSymbols
  val rewritePaths = new RewritePaths
  val relabelUnions = new RelabelUnions
  val pruneFields = new PruneFields
  val resolveZipJoins = new ResolveZipJoins
  val convertToComprehensions = new ConvertToComprehensions
  val fuseComprehensions = new FuseComprehensions
  val fixRowNumberOrdering = new FixRowNumberOrdering
}

/** The mutable state of a compiler run, consisting of immutable state of
  * individual phases that can be updated. */
class CompilationState(val compiler: QueryCompiler) {
  val symbolNamer = new SymbolNamer("s")
  private[compiler] var ast: Node = null
  def tree = ast
  private[this] val state = new HashMap[String, Any]
  def apply[P <: Phase](p: P): p.State = state(p.name).asInstanceOf[p.State]
  def get[P <: Phase](p: P): Option[p.State] = state.get(p.name).asInstanceOf[Option[p.State]]
  def update[S, P <: Phase { type State = S }](p: P, s: S): Unit = state(p.name) = s
  def contains(p: Phase) = state.contains(p.name)
}
