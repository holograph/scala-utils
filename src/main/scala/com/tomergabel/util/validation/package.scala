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
      if ( prototype.size != 1 )
        c.abort( prototype.tail.head.pos, "Only single-parameter validators are supported!" )

      // Extract validation statements --
      case class Subvalidator[ U ]( description: String,
                                    extractor: Expr[ T => U ],
                                    validation: Expr[ Validator[ U ] ] )

      object ValidatorApplication {
        val validatorType = typeOf[ Validator[_] ]
        val function1Type = typeOf[ Function1[_,_] ]
        val contextualizerTerm = typeOf[ Contextualizer[_] ].typeSymbol.name.toTermName

        def patternMatchOn[ R ]( tree: Tree )( pattern: PartialFunction[ Tree, R ] ): Option[ R ] = {
          var found: Option[ R ] = None
          new Traverser {
            override def traverse( subtree: Tree ) {
              if ( pattern.isDefinedAt( subtree ) )
                found = Some( pattern( subtree ) )
              else
                super.traverse( subtree )
            }
          }.traverse( tree )
          found
        }

        def extractObjectUnderValidation( t: Tree ) =
          patternMatchOn( t ) {
            case Apply( TypeApply( Select( _, `contextualizerTerm` ), _ ), e :: Nil ) => e
          } getOrElse
            c.abort( t.pos, s"Failed to extract object under validation from tree $t (raw=${showRaw(t)})" )

        def rewriteSubvalidatorAsExtractor[ U : WeakTypeTag ]( t: Expr[ U ] ): Expr[ T => U ] =
          c.Expr( Function( prototype, t.tree ) )( weakTypeTag[ T => U ] )

        def unapply( expr: Tree ): Option[ Subvalidator[_] ] = expr match {
          case t if t.tpe <:< validatorType =>
            val ouv = extractObjectUnderValidation( expr )
            val ouvtpe = t.tpe.asInstanceOf[ TypeRefApi ].args.head // appliedType( function1Type.typeConstructor, weakTypeOf[ T ] :: t.tpe.asInstanceOf[ TypeRefApi ].args.head :: Nil )
            val ouvttag = c.WeakTypeTag( ouvtpe )
            val ouvexpr = c.Expr( ouv )( ouvttag )
            val extractor = rewriteSubvalidatorAsExtractor( ouvexpr )( ouvttag )
            val svttag = c.WeakTypeTag( t.tpe ) //appliedType( validatorType.typeConstructor, ouv.tpe :: Nil ) )
            val sv = c.Expr( expr )( svttag )
            c.info( ouv.pos, s"""
              |Found subvalidator:
              |  ouv=$ouv
              |  ouvtpe=$ouvtpe
              |  ouvraw=${showRaw(ouv)}
              |  ouvttag=$ouvttag
              |  ouvexpr=$ouvexpr
              |  extractor=$extractor
              |  svttag=$svttag
              |  sv=$sv
              |""".stripMargin, force = false )
            Some( Subvalidator( ouv.toString(), extractor, sv ) )

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

      // Rewrite expressions into a validation chain --

      def rewriteOne[ U ]( sv: Subvalidator[ U ] ): Expr[ Validator[ T ] ] = {
        val descExpr = c.Expr[ String ]( Literal( Constant( sv.description ) ) )

        val applydef = {
          val applypara = newTermName( "v" )
          //            def apply( v: T ) = sv.validation.splice.apply( extractor( v ) ) match {

          val applysel = Apply( sv.validation.tree, Apply( sv.extractor.tree, Ident( applypara ) :: Nil ) :: Nil )

          val successCase = CaseDef( Ident( newTermName( "Success" ) ), EmptyTree, Ident( newTermName( "Success" ) ) )
          val failCase = {
            val vterm = newTermName( "violations" )
            val vexpr = c.Expr[ Seq[ Violation ] ]( Ident( vterm ) )
            val vappl =
              reify { Failure( vexpr.splice map { f => f.copy( constraint = descExpr.splice + " " + f.constraint ) } ) }
            CaseDef(
              Apply( Ident( newTermName( "Failure" ) ), List( Bind( vterm, Ident( nme.WILDCARD ) ) ) ),
              EmptyTree,
              vappl.tree
            )
          }
          val applyimpl = Match( applysel, successCase :: failCase :: Nil )

          DefDef( NoMods, newTermName( "apply" ), Nil,
            List( ValDef( NoMods, applypara, TypeTree( weakTypeOf[ T ] ), EmptyTree ) :: Nil ),
            EmptyTree, applyimpl )
        }

        val anon = c.fresh()
        val cdef = ClassDef( NoMods, anon, Nil,
          Template( TypeTree( weakTypeOf[ Validator[ T ] ] ) :: Nil, emptyValDef, applydef :: Nil ) )
        val ctor = Apply( Select( New( Ident( anon ) ), nme.CONSTRUCTOR ), List.empty )

        val rewrite = c.Expr[ Validator[ T ] ]( Block( cdef :: Nil, ctor ) )

//        val rewrite = reify {
//          new Validator[ T ] {
//            val extractor = sv.extractor.splice
//            def apply( v: T ) = sv.validation.splice.apply( extractor( v ) ) match {
//              case Success => Success
//              case Failure( violations ) =>
//                Failure( violations map { f => f.copy( constraint = descExpr.splice + " " + f.constraint ) } )
//            }
//          }
//        }

        /*
Found subvalidator:
  Description: p.firstName
  Extractor  : Expr[com.tomergabel.util.validation.PrimitiveValidationTests.Person => String](((p: com.tomergabel.util.validation.PrimitiveValidationTests.Person) => p.firstName))
  Validation : Expr[String => com.tomergabel.util.validation.Result](validation.this.`package`.builder.Contextualizer[String](p.firstName).is(validation.this.`package`.builder.notEmpty[String]))
Rewritten as:
  Expr(Block(List(ClassDef(Modifiers(FINAL), newTypeName("$anon"), List(), Template(List(AppliedTypeTree(Select(This(tpnme.PACKAGE), newTypeName("Validator")), List(TypeTree()))), emptyValDef, List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))), ValDef(Modifiers(), newTermName("extractor"), TypeTree(), Function(List(ValDef(Modifiers(PARAM), newTermName("p"), TypeTree(), EmptyTree)), Select(Ident(newTermName("p")), newTermName("firstName")))), DefDef(Modifiers(), newTermName("apply"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("v"), TypeTree(), EmptyTree))), TypeTree(), Match(Apply(Select(Apply(Select(Apply(TypeApply(Select(Select(Select(This(newTypeName("validation")), com.tomergabel.util.validation.package), com.tomergabel.util.validation.builder), newTermName("Contextualizer")), List(TypeTree())), List(Select(Ident(newTermName("p")), newTermName("firstName")))), newTermName("is")), List(TypeApply(Select(Select(Select(This(newTypeName("validation")), com.tomergabel.util.validation.package), com.tomergabel.util.validation.builder), newTermName("notEmpty")), List(TypeTree())))), newTermName("apply")), List(Apply(Select(Select(This(newTypeName("$anon")), newTermName("extractor")), newTermName("apply")), List(Ident(newTermName("v")))))), List(CaseDef(Select(This(tpnme.PACKAGE), newTermName("Success")), EmptyTree, Select(This(tpnme.PACKAGE), newTermName("Success"))), CaseDef(Apply(Select(This(tpnme.PACKAGE), newTermName("Failure")), List(Bind(newTermName("violations"), Ident(nme.WILDCARD)))), EmptyTree, Apply(Select(Select(This(tpnme.PACKAGE), newTermName("Failure")), newTermName("apply")), List(Apply(Apply(Select(Ident(newTermName("violations")), newTermName("map")), List(Function(List(ValDef(Modifiers(PARAM), newTermName("f"), TypeTree(), EmptyTree)), Apply(Select(Ident(newTermName("f")), newTermName("copy")), List(Apply(Select(Apply(Select(Literal(Constant("p.firstName")), newTermName("$plus")), List(Literal(Constant(" ")))), newTermName("$plus")), List(Select(Ident(newTermName("f")), newTermName("constraint")))), Select(Ident(newTermName("f")), newTermName("copy$default$2"))))))), List(Select(Ident(scala.collection.Seq), newTermName("canBuildFrom")))))))))))))), Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List())))
    p.firstName is notEmpty
                ^         */
        c.info( sv.validation.tree.pos,
          s"""|Found subvalidator:
              |  Description: ${sv.description}
              |  Extractor  : ${sv.extractor}
              |  Validation : ${sv.validation}
              |
              |Rewritten as:
              |  Clean      : ${show( rewrite )}
              |  Raw        : ${showRaw( rewrite )}
              |""".stripMargin, force = false )
        rewrite
      }
      val rewritten: List[ Expr[ Validator[ T ] ] ] = subvalidators map { sv =>
        rewriteOne( sv )
      }
      val validators: Expr[ Seq[ Validator[ T ] ] ] = c.Expr[ Seq[ Validator[ T ] ] ](
        Apply( Select( Ident( newTermName( "Seq" ) ), newTermName( "apply" ) ), rewritten map { _.tree } )
      )
      reify { new And( validators.splice :_* ) }

//      reify { new NilValidator }
    }

    def validator[ T ]( v: T => Unit ): Validator[ T ] = macro validator_impl[ T ]

    implicit class Contextualizer[ U ]( value: U ) {
      def is( validator: Validator[ U ] ) = validator
      def has( validator: Validator[ U ] ) = validator
      def are[ E ]( implicit ev: U <:< Traversable[ E ] ) = new {
        def all( validator: Validator[ E ] ) = aggregate( validator, r => ( r fold Success )( _ and _ ) )
      }
    }
    implicit class ExtendValidator[ T ]( validator: Validator[ T ] ) {
      def and( other: Validator[ T ] ) = new And( validator, other ) // TODO shortcut multiple ANDs
      def or( other: Validator[ T ] ) = new Or( validator, other )   // TODO shortcut multiple ORs
    }

    def empty[ T <: HasEmpty ] = new Empty[ T ]
    def notEmpty[ T <: HasEmpty ] = new NotEmpty[ T ]
    def size[ T <: HasSize ] = new Size[ T ]
    def valid[ T ]( implicit validator: Validator[ T ] ) = validator

    private def aggregate[ E, T <: Traversable[ E ] ]( validator: Validator[ E ], aggregator: Traversable[ Result ] => Result ) = new Validator[ T ] {
      def apply( col: T ) = aggregator( col map validator )
    }
  }
}
