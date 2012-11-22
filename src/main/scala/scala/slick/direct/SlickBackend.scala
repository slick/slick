package scala.slick.direct

import scala.slick.SlickException
import scala.language.implicitConversions
import language.existentials
import scala.slick.driver._
import scala.slick.lifted.{Shape, ShapedValue}
import scala.slick.{ast => sq}
import scala.slick.ast.{Library,FunctionSymbol}
import scala.slick.ast.Dump
import scala.slick.util.{CollectionLinearizer,RecordLinearizer,ValueLinearizer}
import scala.reflect.ClassTag
import scala.slick.compiler.CompilationState
import scala.reflect.runtime.universe.TypeRef

trait QueryableBackend

class SlickBackend( val driver: JdbcDriver, mapper:Mapper ) extends QueryableBackend{
  type Session = JdbcDriver#Backend#Session
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror=>cm}

  val columnTypes = Map( // FIXME use symbols instead of strings for type names here
     "Int"              /*typeOf[Int]*/    -> driver.columnTypes.intJdbcType
    ,"Double"           /*typeOf[Double]*/ -> driver.columnTypes.doubleJdbcType
    ,"String"           /*typeOf[String]*/ -> driver.columnTypes.stringJdbcType
    ,"scala.Int"        /*typeOf[Int]*/    -> driver.columnTypes.intJdbcType
    ,"scala.Double"     /*typeOf[Double]*/ -> driver.columnTypes.doubleJdbcType
    ,"scala.String"     /*typeOf[String]*/ -> driver.columnTypes.stringJdbcType
    ,"java.lang.String" /*typeOf[String]*/ -> driver.columnTypes.stringJdbcType // FIXME: typeOf[String] leads to java.lang.String, but param.typeSignature to String
    ,"Boolean"          /*typeBof[Boolean]*/ -> driver.columnTypes.booleanJdbcType
    ,"scala.Boolean"    /*typeBof[Boolean]*/ -> driver.columnTypes.booleanJdbcType
  )

  //def resolveSym( lhs:Type, name:String, rhs:Type* ) = lhs.member(newTermName(name).encodedName).asTerm.resolveOverloaded(actuals = rhs.toList)

  val operatorMap : Vector[ (Map[String, FunctionSymbol], List[List[Type]]) ] = {
    import Library._
    Vector(
      Map( "==" -> Library.== )
      ->
      List(
        List(typeOf[Any]),
        List(typeOf[Any])
      ),
      Map( "+" -> Library.+, "<" -> <, ">" -> > )
      ->
      List(
        List(typeOf[Int],typeOf[Double]),
        List(typeOf[Int],typeOf[Double])
      ),
      Map( "+" -> Concat )
      ->
      List(
        List(typeOf[String],typeOf[java.lang.String]),
        List(typeOf[String],typeOf[java.lang.String])
      ),
      Map( "||" -> <, "&&" -> > )
      ->
      List(
        List(typeOf[Boolean]),
        List(typeOf[Boolean])
      )
    )
  }


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
  def columnType( sym:Symbol ) = columnTypes(sym.typeSignature.typeSymbol.name.decoded)
  def columnField( sym:Symbol ) = sq.FieldSymbol( columnName(sym) )( List(), columnType(sym) )
  def columnSelect( sym:Symbol, sq_symbol:sq.Node ) =
    sq.Select(
      sq.Ref(sq_symbol.nodeIntrinsicSymbol),
      columnField(sym)
    ).nodeTyped( columnType(sym) )

  def typetagToQuery(typetag:TypeTag[_]) : Query = {
    def _fields = getConstructorArgs(typetag.tpe)

    val table = new sq.TableNode with sq.WithOp with sq.TypedNode {
      val schemaName = None
      val tableName = mapper.typeToTable( typetag.tpe )
      def nodeShaped_* = ShapedValue(
        sq.ProductNode( _fields.map( fieldSym => columnSelect(fieldSym,sq.Node(this)) ) ),
        Shape.selfLinearizingShape.asInstanceOf[Shape[sq.ProductNode, Any, _]]
      )
      def tpe = sq.CollectionType(
        sq.CollectionTypeConstructor.default,
        sq.StructType(_fields.map( sym => columnField(sym) -> columnType(sym) ))
      )
      override def nodeWithComputedType(scope: sq.SymbolScope): sq.Node = nodeRebuild
    }
    new Query( table, Scope() )
  }

  def toQuery( tree:Tree, scope : Scope = Scope() ) : (Type,Query) = {
    import scala.tools.reflect._
    // external references (symbols) are preserved by reify, so cm suffices (its class loader does not need to load any new classes)
    val toolbox = cm.mkToolBox()//mkConsoleFrontEnd().asInstanceOf[scala.tools.reflect.FrontEnd],"") // FIXME cast
    val typed_tree = toolbox.typeCheck(tree) // TODO: can we get rid of this to remove the compiler dependency?
    ( typed_tree.tpe, scala2scalaquery_typed( removeTypeAnnotations(typed_tree), scope ) )
  }
  private def eval( tree:Tree ) :Any = tree match {
    case Select(from,name) => {
      val i = cm.reflect( eval(from) )
      val m = i.symbol.typeSignature.member( name ).asMethod
      val mm = i.reflectMethod( m )
      mm()
    }
    case ident:Ident => ident.symbol.asFreeTerm.value
  }
  
  private def scala2scalaquery_typed( tree:Tree, scope : Scope ) : Query = {
    def s2sq( tree:Tree, scope:Scope=scope ) : Query = scala2scalaquery_typed( tree, scope )
    implicit def node2Query(node:sq.Node) = new Query( node, scope )
    try{
      val string_types = List("String","java.lang.String")
      tree match {
        // explicitly state types here until SQ removes type parameters and type mapper from ConstColumn
        case Literal(Constant(x:Int))    => sq.LiteralNode(driver.columnTypes.intJdbcType, x)
        case Literal(Constant(x:String)) => sq.LiteralNode(driver.columnTypes.stringJdbcType, x)
        case Literal(Constant(x:Double)) => sq.LiteralNode(driver.columnTypes.doubleJdbcType, x)
        case ident@Ident(name) if !scope.contains(ident.symbol) => // TODO: move this into a separate inlining step in queryable
          ident.symbol.asFreeTerm.value match {
            case q:BaseQueryable[_] => val (tpe,query) = toQuery( q ); query
            case x => s2sq( Literal(Constant(x)) )
          }
        case ident@Ident(name) => scope(ident.symbol)

        case Select( t, term ) if t.tpe.erasure <:< typeOf[BaseQueryable[_]].erasure && term.decoded == "queryable" => s2sq(t)

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
          
        case Apply( Select( queryOps, term ), queryable::Nil )
          if queryOps.tpe <:< typeOf[QueryOps.type] && queryable.tpe.erasure <:< typeOf[BaseQueryable[_]].erasure && term.decoded == "query"
        => s2sq( queryable ).node
        
        // match queryable methods
        case Apply(Select(scala_lhs,term),Function( arg::Nil, body )::Nil)
          if scala_lhs.tpe.erasure <:< typeOf[QueryOps[_]].erasure
        =>
          val sq_lhs = s2sq( scala_lhs ).node
          val sq_symbol = new sq.AnonSymbol
          val new_scope = scope+(arg.symbol -> sq.Ref(sq_symbol))
          val rhs = s2sq(body, new_scope)
          new Query( term.decoded match {
            case "filter"     => sq.Filter( sq_symbol, sq_lhs, rhs.node )
            case "map"        => sq.Bind( sq_symbol, sq_lhs, sq.Pure(rhs.node) )
            case "flatMap"    => sq.Bind( sq_symbol, sq_lhs, rhs.node )
            case e => throw new UnsupportedMethodException( scala_lhs.tpe.erasure+"."+term.decoded )
          },
            new_scope
          )

        // FIXME: this case is required because of a bug, but should be covered by the next case
        case d@Apply(Select(lhs,term),rhs::Nil)
          if {
            /*println("_a__")
            println(showRaw(d))
            println(showRaw(lhs))
            println(rhs.symbol.asInstanceOf[scala.reflect.internal.Symbols#FreeTerm].value)
            println(rhs.tpe)
            println("_b__")*/
            (
              (string_types contains lhs.tpe.widen.toString) //(lhs.tpe <:< typeOf[String])
                && (string_types contains rhs.tpe.widen.toString) // (rhs.tpe <:< typeOf[String] )
                && (List("+").contains( term.decoded ))
              )
          }
        =>
          term.decoded match {
            case "+" => Library.Concat.typed[String](s2sq( lhs ).node, s2sq( rhs ).node )
          }

        case a@Apply(op@Select(lhs,term),rhs::Nil) => {
          val actualTypes = lhs.tpe :: rhs.tpe :: Nil //.map(_.tpe).toList
          val matching_ops = ( operatorMap.collect{
              case (str2sym, types)
                    if str2sym.isDefinedAt( term.decoded )
                      && types.zipWithIndex.forall{
                           case (expectedTypes, index) => expectedTypes.exists( actualTypes(index) <:< _ ) 
                         }
              => str2sym( term.decoded )
          })
          matching_ops.size match{
            case 0 => throw new SlickException("Operator not supported: "+ lhs.tpe +"."+term.decoded+"("+ rhs.tpe +")")
            case 1 => matching_ops.head.typed(columnTypes(a.tpe.toString), s2sq( lhs ).node, s2sq( rhs ).node )
            case _ => throw new SlickException("Internal Slick error: resolution of "+ lhs.tpe +" "+term.decoded+" "+ rhs.tpe +" was ambigious")
          }
        }
        
        // Tuples
        case Apply(
            Select(Select(Ident(package_), class_), method_),
            components
        )
        if package_.decoded == "scala" && class_.decoded.startsWith("Tuple") && method_.decoded == "apply" // FIXME: match smarter than matching strings
        =>
            sq.ProductNode( components.map(s2sq(_).node) )

        case Select(scala_lhs, term) 
          if scala_lhs.tpe.erasure <:< typeOf[QueryOps[_]].erasure && (term.decoded == "length" || term.decoded == "size")
          => sq.Pure( Library.CountAll.typed[Int](s2sq(scala_lhs).node ) )

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

  private def queryable2cstate[R]( queryable:BaseQueryable[R], session: driver.Backend#Session ) : (Type,CompilationState) = {
    val (tpe,query) = this.toQuery(queryable)
    (tpe,driver.compiler.run(query.node))
  }
  
  private def queryablevalue2cstate[R]( queryablevalue:QueryableValue[R], session:driver.Backend#Session ) : (Type,CompilationState) = {
    val (tpe,query) = this.toQuery(queryablevalue.value.tree)
    (tpe,driver.compiler.run(query.node))
  }

  protected def resultByType( expectedType : Type, rs: PositionedResult, session:driver.Backend#Session) : Any = {
    def createInstance( args:Seq[Any] ) = {
      val constructor = expectedType.member( nme.CONSTRUCTOR ).asMethod
      val cls = cm.reflectClass( cm.classSymbol(cm.runtimeClass(expectedType)) )
      cls.reflectConstructor( constructor )( args:_* )
    }
    import TupleTypes.tupleTypes
    (expectedType match {
      case t if columnTypes.isDefinedAt(expectedType.toString) => driver.typeInfoFor(columnTypes( expectedType.toString )).nextValue(rs)
      case t if tupleTypes.exists( expectedType <:< _ ) =>
        val typeArgs = expectedType match { case TypeRef(_,_,args_) => args_ } 
        val args = typeArgs.map{
          tpe => resultByType( tpe, rs, session )
        }
        createInstance( args )
      case t if t.typeSymbol.asClass.isCaseClass =>
        val args = expectedType.member( nme.CONSTRUCTOR ).typeSignature match {
          case MethodType( params, resultType ) => params.map{ // TODO check that the field order is correct
            param =>  resultByType( param.typeSignature, rs, session )
          }
        }
        createInstance( args )
    })
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
  def result[R]( tpe:Type, cstate:CompilationState, session:driver.Backend#Session) : Vector[R] = {
    val linearizer = new CollectionLinearizer[Vector,R]{
      def elementLinearizer: ValueLinearizer[R] = new RecordLinearizer[R]{
          def getResult(driver: JdbcDriver, rs: PositionedResult): R
            = resultByType( tpe, rs, session ).asInstanceOf[R]
          def updateResult(driver: JdbcDriver, rs: PositionedResult, value: R): Unit = ???
          def setParameter(driver: JdbcDriver, ps: PositionedParameters, value: Option[R]): Unit = ???
          def getLinearizedNodes: IndexedSeq[Node] = ???
        }
        def canBuildFrom: CanBuildFrom[Nothing, R, Vector[R]] = implicitly[CanBuildFrom[Nothing, R, Vector[R]]]
    }
    new driver.QueryExecutor[Vector[R]](new QueryBuilderInput(cstate, linearizer)).run(session)
  }
  protected[slick] def toSql( queryable:BaseQueryable[_], session:driver.Backend#Session ) = {
    val (_,cstate) = queryable2cstate( queryable, session )
    val builder = driver.createQueryBuilder(new QueryBuilderInput(cstate, null))
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
