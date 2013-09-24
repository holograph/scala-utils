package com.tomergabel.util

import scala.language.experimental.macros


/**
 * Created by tomer on 8/7/13.
 */
package object validation {
  // TODO work on the description/messaging infrastructure
  // TODO add negate/or combiners

  case class Violation( constraint: String, value: Any )

  sealed trait Result {
    def and( other: Result ): Result
    def or( other: Result ): Result
  }
  case object Success extends Result {
    def and( other: Result ) = other
    def or( other: Result ) = this
  }
  case class Failure( violations: Seq[ Violation ] ) extends Result {
    def and( other: Result ) = other match {
      case Success => this
      case Failure( vother ) => Failure( violations ++ vother )
    }
    def or( other: Result ) = other match {
      case Success => other
      case Failure( vother ) => Failure( vother )
    }
  }

  type Validator[ T ] = T => Result
  def validate[ T ]( x: T )( implicit validator: Validator[ T ] ) = validator( x )

  object combinators {
    private[ validation ] def result( test: => Boolean, violation: => Violation ) =
      if ( test ) Success else Failure( Seq( violation ) )

    type HasEmpty = { def isEmpty(): Boolean }
    class Empty[ T <: HasEmpty ] extends Validator[ T ] {
      def apply( x: T ) = result( x.isEmpty(), Violation( "must be empty", x ) )
    }

    type HasSize = { def size: Int }
    class Size[ T <: HasSize ] {
      def >( other: Int ) = new Validator[ T ] {
        def apply( x: T ) = result( x.size > other, Violation( s"has size ${x.size}, expected more than $other", x ) )
      }
    }

    class NotEmpty[ T <: HasEmpty ] extends Validator[ T ] {
      def apply( x: T ) = result( !x.isEmpty, Violation( "must not be empty", x ) )
    }

    class And[ T ]( predicates: Validator[ T ]* ) extends Validator[ T ] {
      def apply( x: T ) = predicates.map { _ apply x }.fold( Success ) { _ and _ }
    }

    class Or[ T ]( predicates: Validator[ T ]* ) extends Validator[ T ] {
      def apply( x: T ) = predicates.map { _ apply x }.fold( Success ) { _ or _ }
      // TODO rethink resulting violation
    }

    class Fail[ T ]( message: => String ) extends Validator[ T ] {
      def apply( x: T ) = result( test = false, Violation( message, x ) )
    }

    class NilValidator[ T ] extends Validator[ T ] {
      def apply( x: T ) = Success
    }
  }


  object builder {
    import combinators._

    def validator_impl[ T : c.WeakTypeTag ]( c: scala.reflect.macros.Context )
                                           ( v: c.Expr[ T => Unit ] ): c.Expr[ Validator[ T ] ] = {
      import c.universe._

      // Extract validator body --
      val Function( prototype, impl ) = v.tree

      // Extract validation statements --
      case class Subvalidator[ U ]( description: String,
                                    extractor: Expr[ T => U ],
                                    validation: Expr[ Validator[ U ] ] )

      object ValidatorApplication {
        val validatorType = typeOf[ Validator[_] ]
        val contextualizerTerm = typeOf[ Contextualizer[_] ].typeSymbol.name.toTermName

        def renderTreeSource( t: Tree ) = ( t.pos match {
          // Shamelessly stolen from Expecty's RecorderMacro.
          // https://github.com/pniederw/expecty/blob/master/src/main/scala/org/expecty/RecorderMacro.scala
          case p: Position => p.lineContent
          case p: scala.reflect.internal.util.RangePosition => p.lineContent.slice( p.start, p.end )
        } ).trim

        def extractObjectUnderValidation( t: Tree ) = {
          // TODO cleanup
          var expr: Tree = null
          val lookup = new Traverser {
            override def traverse( tree: Tree ) {
              tree match {
                case Apply( TypeApply( Select( _, term ), _ ), e :: Nil ) => expr = e
                case _ => super.traverse( tree )
              }
            }
          }
          lookup.traverse( t )
          assert( expr != null )
          expr
        }

        def rewriteSubvalidatorAsExtractor[ U : WeakTypeTag ]( t: Expr[ U ] ): Expr[ T => U ] =
          c.Expr( Function( prototype, t.tree ) )( weakTypeTag[ T => U ] )

        def unapply( expr: Tree ): Option[ Subvalidator[_] ] = expr match {
          case t if t.tpe <:< validatorType =>
            val ouv = extractObjectUnderValidation( expr )
            val ouvttag = c.WeakTypeTag( ouv.tpe )
            val ouvexpr = c.Expr( ouv )( ouvttag )
            val extractor = rewriteSubvalidatorAsExtractor( ouvexpr )( ouvttag )
            val svttag = c.WeakTypeTag( appliedType( validatorType.typeConstructor, ouv.tpe :: Nil ) )
            val sv = c.Expr( expr )( svttag )
            Some( Subvalidator( /*renderTreeSource( ouv )*/ ouv.toString(), extractor, sv ) )

          case _ => None
        }
      }

      def findSubvalidators( t: Tree ): List[ Subvalidator[_] ] = t match {
        case Block( stats, expr ) => stats.flatMap( findSubvalidators(_) ) ++ findSubvalidators( expr )
        case ValidatorApplication( validator ) => validator :: Nil
        case Literal( Constant(()) ) => Nil   // Ignored terminator
        case _ => c.abort( t.pos, s"Unexpected node $t:\n\ttpe=${t.tpe}\n\traw=${showRaw(t)}" )
      }

      val subvalidators = findSubvalidators( impl )

      // Rewrite expressions a validation chain --

      def rewriteOne[ U ]( sv: Subvalidator[ U ] ): Expr[ Validator[ T ] ] = {
        c.info( sv.validation.tree.pos, s"Found validation statement '${sv.description}' for tree ${sv.validation}", force = false )
        val descExpr = c.Expr[ String ]( Literal( Constant( sv.description ) ) )
        val rewrite = reify {
          new Validator[ T ] {
            def apply( v: T ) = sv.validation.splice.apply( sv.extractor.splice( v ) ) match {
              case Success => Success
              case Failure( violations ) =>
                Failure( violations map { f => f.copy( constraint = descExpr.splice + " " + f.constraint ) } )
            }
          }
        }
/*
    /*private*/ class Wrapper[ T, U ]( prefix: String, extractor: T => U, validator: Validator[ U ] ) extends Validator[ T ] {
      def apply( v: T ) = validator( extractor( v ) ) match {
        case Success => Success
        case Failure( violations ) =>
          Failure( violations map { f => f.copy( constraint = prefix + " " + f.constraint ) } )
      }
    }
//          reify { new Wrapper[ T, U ]( descExpr.splice, sv.extractor.splice, sv.validation.splice ) }
 */
        c.info( sv.validation.tree.pos, "Rewritten as: " + show( rewrite ), false )
        rewrite
      }
      val rewritten: List[ Expr[ Validator[ T ] ] ] = subvalidators map { rewriteOne(_) }
      val validators: Expr[ Seq[ Validator[ T ] ] ] = c.Expr[ Seq[ Validator[ T ] ] ](
        Apply( Select( Ident( newTermName( "Seq" ) ), newTermName( "apply" ) ), rewritten map { _.tree } )
      )
      reify { new And( validators.splice :_* ) }

//      reify { new NilValidator }
    }

/*
Example 1:
scala:
  boilerplate: Expr(Function(List(ValDef(Modifiers(PARAM), newTermName("p"), TypeTree(), EmptyTree)), Block(List(
  statements:
    Apply(Select(Apply(TypeApply(Select(Select(Select(This(newTypeName("validation")), com.tomergabel.util.validation.package), com.tomergabel.util.validation.builder), newTermName("Contextualizer")), List(TypeTree())), List(Select(Ident(newTermName("p")), newTermName("firstName")))), newTermName("is")), List(TypeApply(Select(Select(Select(This(newTypeName("validation")), com.tomergabel.util.validation.package), com.tomergabel.util.validation.builder), newTermName("notEmpty")), List(TypeTree()))))), Block(List(Apply(Select(Apply(TypeApply(Select(Select(Select(This(newTypeName("validation")), com.tomergabel.util.validation.package), com.tomergabel.util.validation.builder), newTermName("Contextualizer")), List(TypeTree())),
    List(Select(Ident(newTermName("p")), newTermName("lastName")))), newTermName("is")), List(TypeApply(Select(Select(Select(This(newTypeName("validation")), com.tomergabel.util.validation.package), com.tomergabel.util.validation.builder), newTermName("notEmpty")), List(TypeTree()))))), Literal(Constant(()))))))

scala: Expr(Function(List(ValDef(Modifiers(PARAM), newTermName("c"), TypeTree(), EmptyTree)),
Block(List(Apply(Select(Apply(TypeApply(Select(Select(Select(This(newTypeName("validation")), com.tomergabel.util.validation.package), com.tomergabel.util.validation.builder), newTermName("Contextualizer")), List(TypeTree())), List(Select(Ident(newTermName("c")), newTermName("teacher")))), newTermName("is")), List(Apply(TypeApply(Select(Select(Select(This(newTypeName("validation")), com.tomergabel.util.validation.package), com.tomergabel.util.validation.builder), newTermName("valid")), List(TypeTree())), List(Select(This(newTypeName("PrimitiveValidationTests")), newTermName("personValidator")))))), Apply(Select(Apply(TypeApply(Select(Select(Select(This(newTypeName("validation")), com.tomergabel.util.validation.package), com.tomergabel.util.validation.builder), newTermName("allOf")), List(TypeTree())), List(Select(Ident(newTermName("c")), newTermName("students")))), newTermName("are")), List(Apply(TypeApply(Select(Select(Select(This(newTypeName("validation")), com.tomergabel.util.validation.package), com.tomergabel.util.validation.builder), newTermName("valid")), List(TypeTree())), List(Select(This(newTypeName("PrimitiveValidationTests")), newTermName("personValidator"))))))), Block(List(Apply(Select(Apply(TypeApply(Select(Select(Select(This(newTypeName("validation")), com.tomergabel.util.validation.package), com.tomergabel.util.validation.builder), newTermName("Contextualizer")), List(TypeTree())), List(Select(Ident(newTermName("c")), newTermName("students")))), newTermName("has")), List(Apply(Select(TypeApply(Select(Select(Select(This(newTypeName("validation")), com.tomergabel.util.validation.package), com.tomergabel.util.validation.builder), newTermName("size")), List(TypeTree())), newTermName("$greater")), List(Literal(Constant(0))))))), Literal(Constant(()))))))
 */


    def validator[ T ]( v: T => Unit ): Validator[ T ] = macro validator_impl[ T ]

//    def validator[ T ]( v: Validator[ T ]* ): Validator[ T ] = new And( ( v map Prefixer.prefix ):_* )

    implicit class Contextualizer[ U ]( value: U ) {
      def is( validator: Validator[ U ] ) = validator
      def has( validator: Validator[ U ] ) = validator
    }
    implicit class ExtendValidator[ T ]( validator: Validator[ T ] ) {
      def and( other: Validator[ T ] ) = new And( validator, other ) // TODO shortcut multiple ANDs
      def or( other: Validator[ T ] ) = new Or( validator, other )   // TODO shortcut multiple ORs
    }

    def empty[ T <: HasEmpty ] = new Empty[ T ]
    def notEmpty[ T <: HasEmpty ] = new NotEmpty[ T ]
    def size[ T <: HasSize ] = new Size[ T ]
    def valid[ T ]( implicit validator: Validator[ T ] ) = validator

    def aggregate[ T ]( it: Traversable[ T ], aggregator: Traversable[ Result ] => Result ) = new {
      def are( validator: Validator[ T ] ) = new Validator[ Traversable[ T ] ] {
        def apply( c: Traversable[ T ] ) = aggregator( c map validator )
      }
    }

    def allOf[ T ]( it: Traversable[ T ] ) = aggregate( it, r => ( r fold Success )( _ and _ ) )
  }
}
