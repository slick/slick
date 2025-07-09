package slick.tracing

import slick.compiler.{Phase, QueryCompiler}
import slick.ast.{Node, Symbol}
import slick.util.{DumpInfo, TreePrinter}
import io.opentelemetry.api.trace.Span
import scala.collection.mutable

/**
 * Tracing-enabled QueryCompiler that instruments query compilation phases
 * with OpenTelemetry spans for performance monitoring and debugging.
 */
class TracingQueryCompiler(
  phases: IndexedSeq[Phase],
  val tracingConfig: TracingConfig,
  val tracingContext: TracingContext
) extends QueryCompiler(phases) {
  
  /**
   * Runs the query compiler with tracing instrumentation.
   */
  override def run(tree: Node): QueryCompiler.CompilerState = {
    if (!tracingConfig.enabled || !tracingConfig.queryCompilation.enabled) {
      return super.run(tree)
    }
    
    val spanBuilder = tracingContext.createCompilationSpan("query.compile")
    
    tracingContext.withSpanSync(spanBuilder) { span =>
      val startTime = System.nanoTime()
      
      try {
        // Add compilation metadata
        span.setAttribute("slick.compilation.phases.count", phases.size.toLong)
        if (tracingConfig.queryCompilation.includeAstDetails) {
          span.setAttribute("slick.ast.initial.type", tree.getClass.getSimpleName)
          span.setAttribute("slick.ast.initial.toString", tree.toString)
        }
        
        // Track phase execution
        val phaseMetrics = mutable.Map.empty[String, Long]
        
        val result = runWithPhaseTracing(tree, phaseMetrics)
        
        // Record final metrics
        val endTime = System.nanoTime()
        val totalDuration = (endTime - startTime) / 1_000_000
        span.setAttribute("slick.compilation.duration_ms", totalDuration)
        
        if (tracingConfig.queryCompilation.performanceMetrics) {
          phaseMetrics.foreach { case (phaseName, duration) =>
            span.setAttribute(s"slick.compilation.phase.$phaseName.duration_ms", duration)
          }
        }
        
        if (tracingConfig.queryCompilation.includeAstDetails) {
          span.setAttribute("slick.ast.final.type", result.tree.getClass.getSimpleName)
        }
        
        result
      } catch {
        case ex: Exception =>
          span.recordException(ex)
          span.setAttribute("slick.compilation.error", ex.getClass.getSimpleName)
          span.setAttribute("slick.compilation.error_message", ex.getMessage)
          throw ex
      }
    }
  }
  
  /**
   * Runs query compilation with individual phase tracing.
   */
  private def runWithPhaseTracing(
    tree: Node,
    phaseMetrics: mutable.Map[String, Long]
  ): QueryCompiler.CompilerState = {
    var state = new QueryCompiler.CompilerState(tree)
    
    for ((phase, index) <- phases.zipWithIndex) {
      val phaseName = phase.name
      val phaseSpanBuilder = tracingContext.createCompilationSpan(s"phase.$phaseName")
        .setAttribute("slick.compilation.phase.index", index.toLong)
        .setAttribute("slick.compilation.phase.name", phaseName)
      
      if (tracingConfig.queryCompilation.includePhaseDetails) {
        tracingContext.withSpanSync(phaseSpanBuilder) { phaseSpan =>
          val phaseStartTime = System.nanoTime()
          
          try {
            if (tracingConfig.queryCompilation.includeAstDetails) {
              phaseSpan.setAttribute("slick.ast.input.type", state.tree.getClass.getSimpleName)
            }
            
            state = runPhase(phase, state)
            
            val phaseEndTime = System.nanoTime()
            val phaseDuration = (phaseEndTime - phaseStartTime) / 1_000_000
            phaseMetrics(phaseName) = phaseDuration
            
            phaseSpan.setAttribute("slick.compilation.phase.duration_ms", phaseDuration)
            
            if (tracingConfig.queryCompilation.includeAstDetails) {
              phaseSpan.setAttribute("slick.ast.output.type", state.tree.getClass.getSimpleName)
            }
            
          } catch {
            case ex: Exception =>
              phaseSpan.recordException(ex)
              phaseSpan.setAttribute("slick.compilation.phase.error", ex.getClass.getSimpleName)
              throw ex
          }
        }
      } else {
        val phaseStartTime = System.nanoTime()
        state = runPhase(phase, state)
        val phaseEndTime = System.nanoTime()
        val phaseDuration = (phaseEndTime - phaseStartTime) / 1_000_000
        phaseMetrics(phaseName) = phaseDuration
      }
    }
    
    state
  }
  
  /**
   * Creates a new TracingQueryCompiler with additional phases.
   */
  def +(phase: Phase): TracingQueryCompiler = {
    new TracingQueryCompiler(phases :+ phase, tracingConfig, tracingContext)
  }
  
  /**
   * Creates a new TracingQueryCompiler with phases replaced.
   */
  def replace(f: PartialFunction[Phase, Phase]): TracingQueryCompiler = {
    new TracingQueryCompiler(phases.map(p => f.applyOrElse(p, identity[Phase])), tracingConfig, tracingContext)
  }
  
  /**
   * Creates a new TracingQueryCompiler with phases filtered.
   */
  def filter(f: Phase => Boolean): TracingQueryCompiler = {
    new TracingQueryCompiler(phases.filter(f), tracingConfig, tracingContext)
  }
  
  /**
   * Creates a new TracingQueryCompiler with phases removed before the specified phase.
   */
  def removeBefore(phase: Phase): TracingQueryCompiler = {
    val index = phases.indexOf(phase)
    if (index >= 0) {
      new TracingQueryCompiler(phases.drop(index), tracingConfig, tracingContext)
    } else {
      this
    }
  }
  
  /**
   * Creates a new TracingQueryCompiler with phases removed after the specified phase.
   */
  def removeAfter(phase: Phase): TracingQueryCompiler = {
    val index = phases.indexOf(phase)
    if (index >= 0) {
      new TracingQueryCompiler(phases.take(index + 1), tracingConfig, tracingContext)
    } else {
      this
    }
  }
}

/**
 * Companion object for TracingQueryCompiler.
 */
object TracingQueryCompiler {
  
  /**
   * Creates a TracingQueryCompiler with the specified phases.
   */
  def apply(
    phases: Phase*
  )(implicit tracingConfig: TracingConfig, tracingContext: TracingContext): TracingQueryCompiler = {
    new TracingQueryCompiler(phases.toIndexedSeq, tracingConfig, tracingContext)
  }
  
  /**
   * Creates a TracingQueryCompiler from an existing QueryCompiler.
   */
  def fromQueryCompiler(
    queryCompiler: QueryCompiler
  )(implicit tracingConfig: TracingConfig, tracingContext: TracingContext): TracingQueryCompiler = {
    new TracingQueryCompiler(queryCompiler.phases, tracingConfig, tracingContext)
  }
  
  /**
   * Wraps an existing QueryCompiler with tracing capabilities.
   */
  def wrap(
    queryCompiler: QueryCompiler,
    tracingConfig: TracingConfig,
    tracingContext: TracingContext
  ): TracingQueryCompiler = {
    new TracingQueryCompiler(queryCompiler.phases, tracingConfig, tracingContext)
  }
}

/**
 * Tracing-enabled Phase wrapper that adds instrumentation to individual compilation phases.
 */
class TracingPhase(
  val underlying: Phase,
  val tracingConfig: TracingConfig,
  val tracingContext: TracingContext
) extends Phase {
  
  override val name: String = underlying.name
  
  override def apply(state: QueryCompiler.CompilerState): QueryCompiler.CompilerState = {
    if (!tracingConfig.enabled || !tracingConfig.queryCompilation.enabled) {
      return underlying.apply(state)
    }
    
    val spanBuilder = tracingContext.createCompilationSpan(s"phase.$name")
      .setAttribute("slick.compilation.phase.name", name)
    
    tracingContext.withSpanSync(spanBuilder) { span =>
      val startTime = System.nanoTime()
      
      try {
        if (tracingConfig.queryCompilation.includeAstDetails) {
          span.setAttribute("slick.ast.input.type", state.tree.getClass.getSimpleName)
          span.setAttribute("slick.ast.input.symbols", state.symbolNameFor.size.toLong)
        }
        
        val result = underlying.apply(state)
        
        val endTime = System.nanoTime()
        val duration = (endTime - startTime) / 1_000_000
        span.setAttribute("slick.compilation.phase.duration_ms", duration)
        
        if (tracingConfig.queryCompilation.includeAstDetails) {
          span.setAttribute("slick.ast.output.type", result.tree.getClass.getSimpleName)
          span.setAttribute("slick.ast.output.symbols", result.symbolNameFor.size.toLong)
        }
        
        result
      } catch {
        case ex: Exception =>
          span.recordException(ex)
          span.setAttribute("slick.compilation.phase.error", ex.getClass.getSimpleName)
          throw ex
      }
    }
  }
}

/**
 * Companion object for TracingPhase.
 */
object TracingPhase {
  
  /**
   * Wraps a Phase with tracing capabilities.
   */
  def wrap(
    phase: Phase,
    tracingConfig: TracingConfig,
    tracingContext: TracingContext
  ): TracingPhase = {
    new TracingPhase(phase, tracingConfig, tracingContext)
  }
  
  /**
   * Wraps a sequence of phases with tracing capabilities.
   */
  def wrapAll(
    phases: IndexedSeq[Phase],
    tracingConfig: TracingConfig,
    tracingContext: TracingContext
  ): IndexedSeq[TracingPhase] = {
    phases.map(wrap(_, tracingConfig, tracingContext))
  }
}