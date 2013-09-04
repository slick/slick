package scala.slick.direct

import language.existentials
import scala.slick.SlickException
import scala.language.implicitConversions
import scala.slick.driver._
import scala.slick.{ast => sq}
import scala.slick.ast.{Library, FunctionSymbol, Dump, ColumnOption}
import scala.slick.compiler.CompilerState
import scala.reflect.runtime.universe.TypeRef
import scala.annotation.StaticAnnotation
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror=>cm}

/** maps a Scala method to a Slick FunctionSymbol */
final case class slickOp(to:FunctionSymbol) extends StaticAnnotation
/** denotes the Scala type the mapped interface refers to */
final class scalaType[+T](t:Type) extends StaticAnnotation
trait OperationMapping{
  // Supported operators by Slick
  // Slick also supports == for all supported types
  @scalaType[Int](typeOf[Int])
  trait IntOps{
    @ slickOp(Library.+) def +(i:Int)    : Int
    @ slickOp(Library.+) def +(i:Double) : Double
    @ slickOp(Library.<) def <(i:Int)    : Boolean
    @ slickOp(Library.<) def <(i:Double) : Boolean
    @ slickOp(Library.>) def >(i:Int)    : Boolean
    @ slickOp(Library.>) def >(i:Double) : Boolean
  }
  @scalaType[Double](typeOf[Double])
  trait DoubleOps{
    @ slickOp(Library.+) def +(i:Int)    : Double
    @ slickOp(Library.+) def +(i:Double) : Double
    @ slickOp(Library.<) def <(i:Int)    : Boolean
    @ slickOp(Library.<) def <(i:Double) : Boolean
    @ slickOp(Library.>) def >(i:Int)    : Boolean
    @ slickOp(Library.>) def >(i:Double) : Boolean
  }
  @scalaType[Boolean](typeOf[Boolean])
  trait BooleanOps{
    @ slickOp(Library.Not) def unary_! : Boolean
    @ slickOp(Library.Or)  def ||( b:Boolean ) : Boolean
    @ slickOp(Library.And) def &&( b:Boolean ) : Boolean
  }
  //@scalaType[String](typeOf[String]) // <- scalac crash SI-7426
  trait StringOps{
    @ slickOp(Library.Concat) def +(i:String) : String
  }
}

trait QueryableBackend

/** a node for marking reversed columns in sortBy (only used temporarily in this backend) */
object CustomNodes{
  import scala.slick.ast._
  final case class Reverse(value: Node) extends UnaryNode with SimplyTypedNode {
    type Self = Reverse
    def child = value
    override def nodeChildNames = Seq("value")
    protected[this] def nodeRebuild(child: Node) = copy(value = child) // FIXME can we factor this out together with pure?
    protected def buildType = child.nodeType
  }
  final case class Nullsorting(value: Node,sorting:Nullsorting.Sorting) extends UnaryNode with SimplyTypedNode {
    type Self = Nullsorting
    def child = value
    override def nodeChildNames = Seq("value")
    protected[this] def nodeRebuild(child: Node) = copy(value = child) // FIXME can we factor this out together with pure? 
    protected def buildType = child.nodeType
  }
  object Nullsorting extends Enumeration{
    type Sorting = Value
    val First, Last = Value
  }
}
import CustomNodes._

class SlickBackend( val driver: JdbcDriver, mapper:Mapper ) extends QueryableBackend{
  type Session = JdbcDriver#Backend#Session
  import slick.ast.ScalaBaseType
  val columnTypes = {
    Map( // FIXME use symbols instead of strings for type names here
       typeOf[Int].typeSymbol     -> ScalaBaseType.intType
      ,typeOf[Double].typeSymbol  -> ScalaBaseType.doubleType
      ,typeOf[String].typeSymbol  -> ScalaBaseType.stringType
      ,typeOf[Boolean].typeSymbol -> ScalaBaseType.booleanType
    )
  }
  /** generates a map from Scala symbols to Slick FunctionSymbols from description in OperatorMapping */
  val operatorSymbolMap : Map[Symbol,FunctionSymbol] = {
    def annotations[T:TypeTag]( m:Symbol ) = m.annotations.filter{
       case Annotation(tpe,_,_) => tpe <:< typeOf[T]
    }
    typeOf[OperationMapping]
      .members
      // only take annotated members
      .filter(annotations[scalaType[_]](_).size > 0)
      .flatMap(
        _.typeSignature
         .members
         .filter(annotations[slickOp](_).size > 0)
      )
      .map{
        specOp =>
        val scalaType = 
          annotations[scalaType[_]](specOp.owner).head.tpe match{
            case TypeRef(tpe,sym,args) => args.head
          }
        val specOpName = specOp.name
        def argTypeSyms( s:Symbol ) = s.asMethod.paramss.map(_.map(_.typeSignature.typeSymbol))
        // resolve overloaded methods
        scalaType.member(specOpName) 
                 .asTerm
                 .alternatives
                 .find(
                    scalaOp =>{
                      argTypeSyms( scalaOp ) == argTypeSyms( specOp )
                    }
                  )
                  .getOrElse{
                    throw new SlickException("Could not find Scala method: "+scalaType+"."+specOpName+argTypeSyms( specOp ))
                  }
        .->( annotations[slickOp](specOp).head
              match { case Annotation(_,args,_) =>
                          // look up FunctionSymbol from annotation
                          // FIXME: make this simpler
                          val op = args.head.symbol
                          val mod = op.owner.companionSymbol.asModule
                          val i = cm.reflectModule(mod).instance
                          cm.reflect(i).reflectMethod(
                            op.owner.companionSymbol.typeSignature.member(op.name).asMethod
                          )().asInstanceOf[FunctionSymbol]
              }
          )
      }
      .toMap
      .+ ( typeOf[String].member(newTermName("+").encodedName) -> Library.Concat) // workaround for SI-7426
  }

  def isMapped( sym:Symbol ) = operatorSymbolMap.contains(sym) || sym.name.decoded == "==" || sym.name.decoded == "!="

  object removeTypeAnnotations extends Transformer {
    def apply( tree:Tree ) = transform(tree)
    override def transform(tree: Tree): Tree = {
      super.transform {
        tree match {
          case  TypeApply( tree, _ ) => tree
          case  Typed( tree, _ ) => tree
          case tree => tree
        }
      }
    }
  }

  type Scope = Map[Symbol,sq.Node]
  def Scope() : Scope = Map()
  class Query(
               val node : sq.Node,
               val scope : Scope
               )

  def getConstructorArgs( tpe:Type ) =
    tpe.member( nme.CONSTRUCTOR ).typeSignature match {
      case MethodType( params, resultType ) => params // TODO check that the field order is correct
    }

  def columnName( sym:Symbol ) = mapper.fieldToColumn( sym )
  def columnType( tpe:Type ) = {
    val underlying = columnTypes(underlyingTypeSymbol(tpe))
    if( isNullable(tpe) ){
      underlying.optionType
    } else {
      underlying
    }
  }
  private def columnField( sym:Symbol ) = 
    sq.FieldSymbol( columnName(sym) )(
      if( isNullable(sym) )
        List(ColumnOption.Nullable)
      else
        List()
      , columnType(sym.typeSignature)
    )
  private def typeName( sym:Symbol ) : String = sym.name.decoded
  private def typeName( tpe:Type ) : String = typeName( tpe.typeSymbol )
  private def isNullable( sym:Symbol ) = sym == typeOf[Option[_]].typeSymbol 
  private def isNullable( tpe:Type ) : Boolean = isNullable(tpe.typeSymbol) 
  private def underlyingTypeSymbol( tpe:Type ) : Symbol =
    if( isNullable(tpe) )
      tpe match {
        case TypeRef(_,_,args) => args(0).typeSymbol
        case t => throw new Exception("failed to compute underlying type of "+tpe)
      }
    else tpe.typeSymbol
  private def canBeMapped( tpe:Type ) : Boolean = columnTypes.isDefinedAt(underlyingTypeSymbol(tpe))
  private def columnSelect( sym:Symbol, sq_symbol:sq.Node ) =
    sq.Select(
      sq.Ref(sq_symbol.nodeIntrinsicSymbol),
      columnField(sym)
    ).nodeTyped( columnType(sym.typeSignature) )

  def typetagToQuery(typetag:TypeTag[_]) : Query = {
    def _fields = getConstructorArgs(typetag.tpe)
    val tableName = mapper.typeToTable( typetag.tpe )
    val table = sq.TableNode(None, tableName, sq.SimpleTableIdentitySymbol(driver, "_", tableName), null)
    val tableRef = new sq.AnonSymbol
    val tableExp = sq.TableExpansion(tableRef, table, sq.TypeMapping(
      sq.ProductNode( _fields.map( fieldSym => columnSelect(fieldSym, sq.Ref(tableRef)) )),
      v => throw new Exception("not implemented yet"),
      v => cm.reflectClass( cm.classSymbol(cm.runtimeClass(typetag.tpe)) )
        .reflectConstructor(
        typetag.tpe.member( nme.CONSTRUCTOR ).asMethod
      )( (v match {
        case v:Vector[_] => v
        case v:Product => v.productIterator.toVector
      }):_* )
    ))
    new Query( tableExp, Scope() )
  }

  def toQuery( tree:Tree, scope : Scope = Scope() ) : (Type,Query) = {
    import scala.tools.reflect._
    // external references (symbols) are preserved by reify, so cm suffices (its class loader does not need to load any new classes)
    val toolbox = cm.mkToolBox()//mkConsoleFrontEnd().asInstanceOf[scala.tools.reflect.FrontEnd],"") // FIXME cast
    val typed_tree = try
      toolbox.typeCheck(tree) // TODO: can we get rid of this to remove the compiler dependency?
    catch{
      case e:Throwable => println("Failed to typecheck: "+showRaw(tree));throw e 
    }
    ( typed_tree.tpe, scala2scalaquery_typed( removeTypeAnnotations(typed_tree), scope ) )
  }
  private def eval( tree:Tree ) :Any = tree match {
    case Literal(Constant(x)) => x
    case ident:Ident => ident.symbol.asFreeTerm.value
    case Select(from,name) => {
      val i = cm.reflect( eval(from) )
      val m = i.symbol.typeSignature.member( name ).asMethod
      val mm = i.reflectMethod( m )
      mm()
    }
    case o:This if o.symbol.isStatic => cm.reflectModule(o.symbol.companionSymbol.asModule).instance
    case o:This => throw new SlickException( "Cannot handle reference to a query in non-static symbol "+o.symbol )
    case _ => throw new SlickException("Cannot eval: " + showRaw(tree))
  }
  
  private def scala2scalaquery_typed( tree:Tree, scope : Scope ) : Query = {
    def s2sq( tree:Tree, scope:Scope=scope ) : Query = scala2scalaquery_typed( tree, scope )
    def mapOp( op:Tree, args:List[Tree] ) : sq.Node = {
      val Select(lhs:Tree, term:Name) = op
      if( term.decoded == "!=" ){
        Library.Not.typed(
         columnTypes(typeOf[Boolean].typeSymbol),
         mapOp( Select(lhs, newTermName("==")), args )
       )
      } else {
        val (slickOp,slickType) = 
          if(term.decoded == "=="){
            Library.== -> columnTypes(typeOf[Boolean].typeSymbol)
          } else {
            val sym = op.symbol.asMethod
            if( !operatorSymbolMap.keys.toList.contains(sym) ){
              throw new SlickException("Direct embedding does not support method "+sym.owner.name+"."+sym.name.decoded+sym.paramss.map(_.map(_.typeSignature.normalize)).mkString("").toString.replace("List","")+":"+sym.returnType)
            }
            operatorSymbolMap( sym ) -> columnTypes(sym.returnType.typeSymbol)
          }
        slickOp.typed( slickType, (lhs::args).map(s2sq(_).node) : _* )
      }
    }
    implicit def node2Query(node:sq.Node) = new Query( node, scope )
    try{
      val string_types = List("String","java.lang.String")
      tree match {
        // explicitly state types here until SQ removes type parameters and type mapper from LiteralColumn
        case Literal(Constant(x:Int))    => sq.LiteralNode(driver.columnTypes.intJdbcType, x)
        case Literal(Constant(x:String)) => sq.LiteralNode(driver.columnTypes.stringJdbcType, x)
        case Literal(Constant(x:Double)) => sq.LiteralNode(driver.columnTypes.doubleJdbcType, x)
        case ident@Ident(name) if !scope.contains(ident.symbol) => // TODO: move this into a separate inlining step in queryable
          ident.symbol.asFreeTerm.value match {
            case q:BaseQueryable[_] => val (tpe,query) = toQuery( q ); query
            case x => s2sq( Literal(Constant(x)) )
          }
        case ident@Ident(name) => scope(ident.symbol)

        case op@Select( t, _ ) if op.symbol == typeOf[BaseQueryable[_]].member(newTermName("queryable")) => s2sq(t)

        // match columns
        case Select(from,name) if mapper.isMapped( from.tpe.widen )
        =>
          columnSelect( getConstructorArgs( from.tpe.widen ).filter(_.name==name).head, scope(from.symbol) )
/*
        // TODO: Where is this needed?
        case Select(a:This,b) =>
          val obj = companionInstance( a.symbol )
          val value = invoke( obj, a.tpe.nonPrivateMember(b) )()
          value match{
            case q:BaseQueryable[_] => toQuery( q )
            case x => s2sq( Literal(Constant(x)) )
          }
*/
        case a@Apply(op@Select(lhs,term),args) if isMapped( op.symbol )
          => mapOp(op,args)

        case op@Select(lhs,term) if isMapped( op.symbol )
          => mapOp(op,List())
                  
        case Apply( op, queryable::Nil )
          if op.symbol == typeOf[QueryOps.type].member(newTermName("query"))
            => s2sq( queryable ).node
        
        // match queryable methods
        case op@Select(scala_lhs, term) if typeOf[QueryOps[_]].members.toList.contains(op.symbol) =>
          term.decoded match {
            case "length" => sq.Pure( Library.CountAll.typed[Int](s2sq(scala_lhs).node ) )
          }

        case Apply(op@Select(scala_lhs,term),args) if typeOf[QueryOps[_]].members.toList.contains(op.symbol)
        =>
          val (rhs::Nil) = args
          val sq_lhs = s2sq( scala_lhs ).node
          val sq_symbol = new sq.AnonSymbol
          def flattenAndPrepareForSortBy( node:sq.Node ) : Seq[(sq.Node,sq.Ordering)] = node match {
            case sq.ProductNode(nodes) => nodes.flatMap(flattenAndPrepareForSortBy _)
            case Reverse(node)                                => Seq( (node,sq.Ordering(sq.Ordering.Desc)) )
            case Nullsorting(Reverse(node),Nullsorting.First) => Seq( (node,sq.Ordering(sq.Ordering.Desc,sq.Ordering.NullsFirst)) )
            case Nullsorting(node,Nullsorting.First)          => Seq( (node,sq.Ordering(sq.Ordering.Asc ,sq.Ordering.NullsFirst)) )
            case Nullsorting(Reverse(node),Nullsorting.Last)  => Seq( (node,sq.Ordering(sq.Ordering.Desc,sq.Ordering.NullsLast)) )
            case Nullsorting(node,Nullsorting.Last)           => Seq( (node,sq.Ordering(sq.Ordering.Asc ,sq.Ordering.NullsLast)) )
            case node => Seq( (node,sq.Ordering(sq.Ordering.Asc)) )
          }
          rhs match {
            case Function( arg::Nil, body ) =>
              val new_scope = scope+(arg.symbol -> sq.Ref(sq_symbol))
              val sq_rhs = s2sq(body, new_scope).node
              new Query( term.decoded match {
                case "filter"     => sq.Filter.ifRefutable( sq_symbol, sq_lhs, sq_rhs )
                case "sortBy"     => sq.SortBy( sq_symbol, sq_lhs, flattenAndPrepareForSortBy(sq_rhs) )
                case "map"        => sq.Bind( sq_symbol, sq_lhs, sq.Pure(sq_rhs) )
                case "flatMap"    => sq.Bind( sq_symbol, sq_lhs, sq_rhs )
                case e => throw new UnsupportedMethodException( scala_lhs.tpe.erasure+"."+term.decoded )
              },
              new_scope
            )
            case _ => new Query( term.decoded match {
                case "drop"       =>
                  val i = eval(rhs)
                  if( !i.isInstanceOf[Int] ){
                    throw new Exception("drop expects Int, found "+i.getClass)
                  }
                  sq.Drop( sq_lhs, i.asInstanceOf[Int] ) 
                case "take"       =>
                  val i = eval(rhs)
                  if( !i.isInstanceOf[Int] ){
                    throw new Exception("take expects Int, found "+i.getClass)
                  }
                  sq.Take( sq_lhs, i.asInstanceOf[Int] ) 
                case e => throw new UnsupportedMethodException( scala_lhs.tpe.erasure+"."+term.decoded )
              },
              scope
            )
          }

        // Tuples
        case Apply(
            op,
            components
        )
        if definitions
            .TupleClass
            .filter(_ != NoSymbol)
            .map( _.companionSymbol.typeSignature.member( newTermName("apply") ) )
            .contains( op.symbol )
        =>
            sq.ProductNode( components.map(s2sq(_).node) )

        case Apply( op, scala_rhs::Nil )
          if typeOf[NullAndReverseOrder].member(newTermName("nonesLast")).asTerm.alternatives.contains(op.symbol)
            => Nullsorting( s2sq(scala_rhs).node, Nullsorting.Last )

        case Apply( op, scala_rhs::Nil )
          if typeOf[NullAndReverseOrder].member(newTermName("nonesFirst")).asTerm.alternatives.contains(op.symbol)
            => Nullsorting( s2sq(scala_rhs).node, Nullsorting.First )

        case Apply( op, scala_rhs::Nil )
          if typeOf[NullAndReverseOrder].member(newTermName("reversed")).asTerm.alternatives.contains(op.symbol)
            => Reverse( s2sq(scala_rhs).node )

        case tree if tree.tpe.erasure <:< typeOf[BaseQueryable[_]].erasure
            => val (tpe,query) = toQuery( eval(tree).asInstanceOf[BaseQueryable[_]] ); query

        case tree => throw new Exception( "You probably used currently not supported scala code in a query. No match for:\n" + showRaw(tree) )
      }
    } catch{
      case e:java.lang.NullPointerException => { println("NPE in tree "+showRaw(tree));throw e}
    }
  }
  protected[slick] def dump( queryable:BaseQueryable[_] ) = {
    val (_,query) = this.toQuery(queryable)
    Dump(query.node)
  }
  import scala.collection.generic.CanBuildFrom
  import scala.slick.jdbc.{PositionedParameters, PositionedResult}
  import scala.slick.ast.Node

  private def queryable2cstate[R]( queryable:BaseQueryable[R], session: driver.Backend#Session ) : (Type,CompilerState) = {
    val (tpe,query) = this.toQuery(queryable)
    (tpe,driver.queryCompiler.run(query.node))
  }
  
  private def queryablevalue2cstate[R]( queryablevalue:QueryableValue[R], session:driver.Backend#Session ) : (Type,CompilerState) = {
    val (tpe,query) = this.toQuery(queryablevalue.value.tree)
    (tpe,driver.queryCompiler.run(query.node))
  }
  def result[R]( queryable:BaseQueryable[R], session:driver.Backend#Session) : Vector[R] = {
    val (tpe,query) = queryable2cstate( queryable, session )
    result(tpe,query, session)
  }
  def result[R]( queryablevalue:QueryableValue[R], session:driver.Backend#Session) : R = {
    val (tpe,query) = queryablevalue2cstate( queryablevalue, session )
    val res = result(tpe,query, session)
    res(0)
  }
  def result[R]( tpe:Type, cstate:CompilerState, session:driver.Backend#Session) : Vector[R] =
    new driver.QueryExecutor[Vector[R]](cstate.tree, ()).run(session)

  protected[slick] def toSql( queryable:BaseQueryable[_], session:driver.Backend#Session ) = {
    val (_,cstate) = queryable2cstate( queryable, session )
    val builder = driver.createQueryBuilder(cstate.tree, cstate)
    builder.buildSelect.sql
  }
  protected[slick] def toQuery(queryable:BaseQueryable[_]) : (Type,this.Query) = queryable.expr_or_typetag match {
    case Right((typetag,classtag)) => (typetag.tpe, this.typetagToQuery( typetag ))
    case Left(expr_)    =>
        val (tpe,query) = this.toQuery(expr_.tree)
        val args = tpe match { case TypeRef(_,_,args_) => args_ } 
        (args(0), query)
  }

  def toList[T]( queryable:BaseQueryable[T] ) : List[T] = {
    import this.driver.Implicit._
    val (_,query) = this.toQuery(queryable)
    val node = query.node : scala.slick.ast.Node
    null
  }
}
