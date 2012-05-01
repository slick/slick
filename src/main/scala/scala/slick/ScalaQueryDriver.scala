package scala.slick
import language.implicitConversions

import driver._
import driver.{ExtendedTable => Table}
import ql._
import slick.{ast => sq}

trait QueryableBackend

class SlickBackend(driver:BasicDriver) extends QueryableBackend{
  import scala.reflect.mirror._
  
  object removeTypeAnnotations extends reflect.mirror.Transformer {
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
  
  type Scope = Map[Symbol,sq.Symbol]
  def Scope() : Scope = Map() 
  class Query(
    val node : sq.Node,
    val scope : Scope
  )
  // // Why does this not work?
  //      invoke( n, classToType( n.getClass ).nonPrivateMember(newTermName("generator")) ).asInstanceOf[sq.Symbol]
  def symbol2type( s:Symbol ) : Type = classToType(symbolToClass(s))
  def classToQuery[T:reflect.ConcreteTypeTag] : Query = typetagToQuery( typeTag[T] )
  def typetagToQuery(typetag:reflect.mirror.TypeTag[_]) : Query = {
    val scala_symbol = classToSymbol(typetag.erasure)
    val table = 
      new Table[Nothing]({
        val ants = scala_symbol.annotations
            ants match {
            case AnnotationInfo(tpe,tree,_) :: Nil // FIXME:<- don't match list, match any annotation
            //if tpe <:< classToType(classOf[table]) // genJVM bug
            =>
              {
                val name = tree(0).toString
                name.slice( 1,name.length-1 ) // FIXME: <- why needed?
              }
            case a => throw new Exception("Type argument passed to Queryable.apply needs database mapping annotations. None found on: " + typetag.erasure.toString )
        }
      }){def * = ???}
    
    val sq_symbol = new sq.AnonSymbol

    val columns = 
      classToType( typetag.erasure ).widen.members.collect{
        case member if member.annotations.size > 0 && member.annotations.exists{
          case x@AnnotationInfo(tpe,tree,_)
            if tpe <:< classToType(classOf[column])
              => true
        } => member.annotations.collect{
          case x@AnnotationInfo(tpe,tree,_)
            if tpe <:< classToType(classOf[column])
          =>{ // FIXME: is this the right way to do it?
              val name = tree(0).toString
              name.slice( 1,name.length-1 ) // FIXME: <- why needed?
            }
        }.head
      }.map(
        column_name =>
          sq.FieldRef(sq_symbol, sq.FieldSymbol(column_name)(Some(RawNamedColumn(column_name,List(),null))) )
      ).toSeq

    new Query( sq.Bind(sq_symbol, table, sq.Pure(sq.ProductNode(columns:_*))), Scope() )
  }
/*  def apply( tree:Tree, queryable:Queryable[_] ) : Query = {
    this.apply(tree,queryable.query.scope)
  }*/
  def toQuery( tree:Tree, scope : Scope = Scope() ) : Query = {
    val toolbox = mkToolBox(mkConsoleFrontEnd(),"")
//    val typed_tree = toolbox.typeCheck(tree.asInstanceOf[reflect.runtime.Mirror.Tree]  ).asInstanceOf[reflect.mirror.Tree]
    val typed_tree = toolbox.typeCheck(tree)
    scala2scalaquery_typed( removeTypeAnnotations(typed_tree), scope )
  }
  private def scala2scalaquery_typed( tree:Tree, scope : Scope ) : Query = {
    def s2sq( tree:Tree, scope:Scope=scope ) : Query = scala2scalaquery_typed( tree, scope )
    implicit def node2Query(node:sq.Node) = new Query( node, scope )
    try{
      tree match {
        // explicitly state types here until SQ removes type parameters and type mapper from ConstColumn 
        case Literal(Constant(x:Int))    => ConstColumn[Int](x)
        case Literal(Constant(x:String)) => ConstColumn[String](x)
        case Literal(Constant(x:Double)) => ConstColumn[Double](x)
  
       case node@Ident(name) if node.symbol.isInstanceOf[scala.reflect.internal.Symbols#FreeTerm] => // TODO: move this into a separate inlining step in queryable
         node.symbol.asInstanceOf[scala.reflect.internal.Symbols#FreeTerm].value match{
            case q:Queryable[_] => toQuery( q )
            case x => s2sq( Literal(Constant(x)) )
          }
  
       case node@Ident(name) => {
         val sq_symbol = scope(node.symbol)
         sq.Ref(sq_symbol) // FIXME: this is probably wrong. what should go here?
       }
       
        // match columns
        case Select(from,name) 
          if {
            val annotations = from.tpe.widen.typeSymbol.annotations
            annotations.length > 0 && (annotations match {
              case AnnotationInfo(tpe,_,_) :: Nil
                if tpe <:< classToType(classOf[table])
                => true
              case _ => false
            })
          }
          =>
          val sq_symbol= scope(from.symbol)
          val type_ = from.tpe.widen
          val member = type_.members.filter(_.name == name).toList(0)
          val column_name = member.annotations match {
              case x@AnnotationInfo(_,tree,_) :: Nil => 
                { // FIXME: is this the right way to do it?
                  val name = tree(0).toString
                  name.slice( 1,name.length-1 ) // FIXME: <- why needed?
                }
              case a => throw new Exception(member.toString) // FIXME
            }
          sq.FieldRef(sq_symbol, sq.FieldSymbol(column_name)(Some(RawNamedColumn(column_name,List(),null))) )
  
        case Select(a:This,b) =>
          val obj = companionInstance( a.symbol )
          val value = invoke( obj, a.tpe.nonPrivateMember(b) )()
          value match{
            case q:Queryable[_] => toQuery( q )
            case x => s2sq( Literal(Constant(x)) )
          }
          
        // match queryable methods
        case Apply(Select(scala_lhs,term),Function( arg::Nil, body )::Nil) 
          if scala_lhs.tpe.erasure <:< classToType(classOf[Queryable[_]]).erasure
          => 
          val sq_lhs = s2sq( scala_lhs ).node
          val sq_symbol = new sq.AnonSymbol
          val new_scope = scope+(arg.symbol -> sq_symbol)
          val rhs = s2sq(body, new_scope)
          new Query( term.decoded match {
                  case "_filter_placeholder"     => sq.Filter( sq_symbol, sq_lhs, rhs.node )
                  case "_map_placeholder"        => sq.Bind( sq_symbol, sq_lhs, sq.Pure(rhs.node) )
                  case "_flatMap_placeholder"    => sq.Bind( sq_symbol, sq_lhs, rhs.node )
                  case e => throw new UnsupportedMethodException( scala_lhs.tpe.erasure+"."+term.decoded )
                },
                new_scope
            )
  
          // match scalar operators
        case Apply(Select(lhs,term),rhs::Nil)
          if lhs.tpe <:< classToType( classOf[Boolean] )
          && rhs.tpe <:< classToType( classOf[Boolean] )
          && List("||", "&&").contains( term.decoded )
          =>
          ColumnOps.Relational(term.decoded, s2sq( lhs ).node, s2sq( rhs ).node )
  
        case Apply(Select(lhs,term),rhs::Nil)
          if (lhs.tpe <:< classToType( classOf[Int] )
               && rhs.tpe <:< classToType( classOf[Int] )
             ) || ( lhs.tpe <:< classToType( classOf[Double] )
               && rhs.tpe <:< classToType( classOf[Double] )
             )
          && List("+").contains( term.decoded )
          =>
          ColumnOps.Relational(term.decoded, s2sq( lhs ).node, s2sq( rhs ).node )
  
        case d@Apply(Select(lhs,term),rhs::Nil)
          if {
            /*println("_a__")
            println(showRaw(d))
            println(showRaw(lhs))
            println(rhs.symbol.asInstanceOf[scala.reflect.internal.Symbols#FreeTerm].value)
            println(rhs.tpe)
            println("_b__")*/
          (
            (lhs.tpe <:< classToType( classOf[String] ))
            && (rhs.tpe <:< classToType( classOf[String] ))
            && (List("+").contains( term.decoded ))
          )
          }
          =>
          term.decoded match {
            case "+" => ColumnOps.Relational("concat", s2sq( lhs ).node, s2sq( rhs ).node )
          }
  
        case Apply(Select(lhs,term),rhs::Nil)
          if List("<",">","==","!=").contains( term.decoded )
          =>
          ColumnOps.Relational(term.decoded, s2sq( lhs ).node, s2sq( rhs ).node )
  
  /*        
        // match other methods
        case Apply(Select(lhs,term),rhs::Nil)
          =>
          throw new UnsupportedMethodException( lhs.tpe.erasedType+"."+term.decoded+"("+rhs.tpe.erasedType+")" )
  */
  
        case tree => /*Expr[Any](tree).eval match{
            case q:Queryable[_] => q.query
            case x => s2sq( Literal(Constant(x)) )
          }*/
        throw new Exception( "no match for: " + showRaw(tree) )
        
      }
    } catch{
      case e:java.lang.NullPointerException => { println("NPE in tree "+showRaw(tree));throw e}
    }
  }
  protected[slick] def dump( queryable:Queryable[_] ) = {
    val query = this.toQuery(queryable)
    sq.Node(query.node).dump("")
  }
  protected[slick] def toSql( queryable:Queryable[_] ) = {
    val query = this.toQuery(queryable)
    import driver._
    val node = processAST(query.node)
    sq.AnonSymbol.assignNames( node )
    val builder = new QueryBuilder( node, null )
    builder.buildSelect.sql   
  }
  protected[slick] def toQuery(queryable:Queryable[_]) : this.Query = queryable.expr_or_typetag match {
    case Right(typetag) => this.typetagToQuery( typetag )
    case Left(expr_)    => this.toQuery(expr_.tree)
  }
  
  def toList[T]( queryable:Queryable[T] ) : List[T] = {
    import this.driver.Implicit._
    val node = this.toQuery(queryable).node : scala.slick.ast.Node
    null
  }
}
