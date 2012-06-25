package scala.slick.queryable

import scala.language.implicitConversions
import scala.slick.driver._
import scala.slick.driver.BasicDriver.Table
import scala.slick.ql._
import scala.slick.{ast => sq}
import scala.slick.ast.Library
import scala.slick.ast.Dump
import scala.slick.util.{CollectionLinearizer,RecordLinearizer,ValueLinearizer}
import scala.slick.session.{Session}
import scala.reflect.ClassTag

trait QueryableBackend

class SlickBackend(driver:BasicDriver) extends QueryableBackend{
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror=>cm}

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
  // // Why does this not work?
  //      invoke( n, classToType( n.getClass ).nonPrivateMember(newTermName("generator")) ).asInstanceOf[sq.Symbol]

  def getConstructorArgs( tpe:Type ) =
    tpe.member( nme.CONSTRUCTOR ).typeSignature match {
      case MethodType( params, resultType ) => params // TODO check that the field order is correct
    }

  def extractColumn( sym:Symbol, sq_symbol:sq.Node ) = {
    val column_name = sym.getAnnotations.collect{
      case x@AnnotationInfo(tpe,tree,_)
          if tpe <:< typeOf[column]
        => { // FIXME: is this the right way to do it?
          val name = tree(0).toString
            name.slice( 1,name.length-1 ) // FIXME: <- why needed?
        }
    }.head

    sq.Select(sq_symbol, sq.FieldSymbol(column_name)(List(),null))
  }

  def typetagToQuery(typetag:TypeTag[_]) : Query = {
    val scala_symbol = typetag.tpe.typeSymbol

/*
      // DISABLED get column names from memer annotations
      typetag.tpe.widen.members.collect{
        case member if member.getAnnotations.size > 0 && member.getAnnotations.exists{
          case x@AnnotationInfo(tpe,tree,_)
            if tpe <:< typeOf[column]
          => true
        } => member.getAnnotations.collect{
          case x@AnnotationInfo(tpe,tree,_)
            if tpe <:< typeOf[column]
          =>{ // FIXME: is this the right way to do it?
          val name = tree(0).toString
            name.slice( 1,name.length-1 ) // FIXME: <- why needed?
          }
        }.head
      }.map(
        column_name =>
          sq.Select(sq.Ref(sq_symbol), sq.FieldSymbol(column_name)(List(),null))
      ).toSeq*/
    // get column names from constructor arg annotations

    val table = new sq.TableNode with sq.NullaryNode with sq.WithOp {
      val tableName = {
        val ants = scala_symbol.getAnnotations
        ants match {
          case AnnotationInfo(tpe,tree,_) :: Nil // FIXME:<- don't match list, match any annotation
            //if tpe <:< classToType(classOf[table]) // genJVM bug
          =>
          {
            val name = tree(0).toString
            name.slice( 1,name.length-1 ) // FIXME: <- why needed?
          }
          case a => throw new Exception("Type argument passed to Queryable.apply needs database mapping annotations. None found on: " + typetag.tpe.toString )
        }
      }
      def columns = getConstructorArgs( typetag.tpe ).map{extractColumn(_,sq.Node(this))} // use def here, not val, so expansion is still correct after cloning

      def nodeShaped_* = ShapedValue(sq.ProductNode(columns), Shape.selfLinearizingShape.asInstanceOf[Shape[sq.ProductNode, Any, _]])
    }

    
    new Query( table, Scope() )
  }
  /*  def apply( tree:Tree, queryable:Queryable[_] ) : Query = {
    this.apply(tree,queryable.query.scope)
  }*/
  def toQuery( tree:Tree, scope : Scope = Scope() ) : Query = {
    import scala.tools.reflect._
    // external references (symbols) are preserved by reify, so cm suffices (its class loader does not need to load any new classes)
    val toolbox = cm.mkToolBox()//mkConsoleFrontEnd().asInstanceOf[scala.tools.reflect.FrontEnd],"") // FIXME cast
    val typed_tree = toolbox.typeCheck(tree) // TODO: can we get rid of this to remove the compiler dependency?
    scala2scalaquery_typed( removeTypeAnnotations(typed_tree), scope )
  }
  private def scala2scalaquery_typed( tree:Tree, scope : Scope ) : Query = {
    def s2sq( tree:Tree, scope:Scope=scope ) : Query = scala2scalaquery_typed( tree, scope )
    implicit def node2Query(node:sq.Node) = new Query( node, scope )
    try{
      val string_types = List("String","java.lang.String")
      tree match {
        // explicitly state types here until SQ removes type parameters and type mapper from ConstColumn 
        case Literal(Constant(x:Int))    => ConstColumn[Int](x)
        case Literal(Constant(x:String)) => ConstColumn[String](x)
        case Literal(Constant(x:Double)) => ConstColumn[Double](x)
        case ident@Ident(name) if !scope.contains(ident.symbol) => // TODO: move this into a separate inlining step in queryable
          ident.symbol.asFreeTermSymbol.value match {
            case q:Queryable[_] => toQuery( q )
            case x => s2sq( Literal(Constant(x)) )
          }
        case ident@Ident(name) => scope(ident.symbol)

        // match columns
        case Select(from,name)
          if {
            val annotations = from.tpe.widen.typeSymbol.getAnnotations
            annotations.length > 0 && (annotations match {
              case AnnotationInfo(tpe,_,_) :: Nil
                if tpe <:< typeOf[table]
              => true
              case _ => false
            })
          }
        =>
          extractColumn( getConstructorArgs( from.tpe.widen ).filter(_.name==name).head, scope(from.symbol) )
          
/*
        // TODO: Where is this needed?
        case Select(a:This,b) =>
          val obj = companionInstance( a.symbol )
          val value = invoke( obj, a.tpe.nonPrivateMember(b) )()
          value match{
            case q:Queryable[_] => toQuery( q )
            case x => s2sq( Literal(Constant(x)) )
          }
*/

        // match queryable methods
        case Apply(Select(scala_lhs,term),Function( arg::Nil, body )::Nil)
          if scala_lhs.tpe.erasure <:< typeOf[Queryable[_]].erasure
        =>
          val sq_lhs = s2sq( scala_lhs ).node
          val sq_symbol = new sq.AnonSymbol
          val new_scope = scope+(arg.symbol -> sq.Ref(sq_symbol))
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
          if lhs.tpe <:< typeOf[Boolean]
            && rhs.tpe <:< typeOf[Boolean]
            => term.decoded match {
              case "||" => Library.Or ( s2sq( lhs ).node, s2sq( rhs ).node )
              case "&&" => Library.And( s2sq( lhs ).node, s2sq( rhs ).node )
            }

        case Apply(Select(lhs,term),rhs::Nil)
          if ((lhs.tpe <:< typeOf[Int]
            && rhs.tpe <:< typeOf[Int]
            ) || ( lhs.tpe <:< typeOf[Double]
            && rhs.tpe <:< typeOf[Double]
            ))
            && List("+").contains( term.decoded )
        =>
          Library.+(s2sq( lhs ).node, s2sq( rhs ).node)//( typeMappers(rhs.tpe.widen.toString) )

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
            case "+" => Library.Concat(s2sq( lhs ).node, s2sq( rhs ).node )
          }

        case Apply(Select(lhs,term),rhs::Nil)
          if List("<",">","==","!=").contains( term.decoded )
        =>
          (term.decoded match{
            case "<" => Library.<
            case ">" => Library.>
//            case "!" => Library.Not
//            case "!=" => Library.!=
            case "==" => Library.==
          })(s2sq( lhs ).node, s2sq( rhs ).node )

        /*
              // match other methods
              case Apply(Select(lhs,term),rhs::Nil)
                =>
                throw new UnsupportedMethodException( lhs.tpe.erasedType+"."+term.decoded+"("+rhs.tpe.erasedType+")" )
        */
        case Apply(Select(lhs,term),rhs::Nil) => throw new Exception("cannot handle"+(lhs.tpe,term.decoded, rhs.tpe))
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
    Dump(query.node)
  }
  protected[slick] def toSql( queryable:Queryable[_] ) = {
    val query = this.toQuery(queryable)
    import driver._
    val node = sq.opt.Relational(query.node)
    sq.AnonSymbol.assignNames( node )
    val builder = new QueryBuilder( node, null )
    builder.buildSelect.sql
  }
  protected[slick] def toQuery(queryable:Queryable[_]) : this.Query = queryable.expr_or_typetag match {
    case Right((typetag,classtag)) => this.typetagToQuery( typetag )
    case Left(expr_)    => this.toQuery(expr_.tree)
  }

  def toList[T]( queryable:Queryable[T] ) : List[T] = {
    import this.driver.Implicit._
    val node = this.toQuery(queryable).node : scala.slick.ast.Node
    null
  }
}
