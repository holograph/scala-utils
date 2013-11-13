package com.tomergabel.util.validation

import scala.reflect.macros.Context

private class ValidationTransform[ C <: Context, T : C#WeakTypeTag ]( val context: C, v: C#Expr[ T => Unit ] ) {
  import context.universe._
  import context.{abort, info}


  // Macro helpers --

  def extractFromPattern[ R ]( tree: Tree )( pattern: PartialFunction[ Tree, R ] ): Option[ R ] = {
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

  def transformByPattern( tree: Tree )( pattern: PartialFunction[ Tree, Tree ] ): Tree =
    new Transformer {
      override def transform( subtree: Tree ): Tree =
        if ( pattern isDefinedAt subtree ) pattern.apply( subtree ) else super.transform( subtree )
    }.transform( tree.duplicate )

  def defaultCtor( argsToSuper: List[ Tree ] = Nil ) = {
    DefDef(
      mods     = NoMods,
      name     = nme.CONSTRUCTOR,
      tparams  = Nil,
      vparamss = List( List.empty ),
      tpt      = TypeTree(),
      rhs      = Block(
        Apply( Select( Super( This( tpnme.EMPTY ), tpnme.EMPTY ), nme.CONSTRUCTOR ), argsToSuper ) :: Nil,
        Literal( Constant( () ) ) )
    )
  }

  implicit class ListOfExprConversions[ E : WeakTypeTag ]( seq: List[ Expr[ E ] ] ) {
    def consolidate: Expr[ Seq[ E ] ] =
      context.Expr[ Seq[ E ] ](
        Apply( Select( Ident( newTermName( "Seq" ) ), newTermName( "apply" ) ),
        seq map { _.tree } )
      )
  }


  private val verboseValidatorRewrite = context.settings.contains( "verboseValidationTransform" )
  def log( s: String, pos: Position = context.enclosingPosition ) =
    if ( verboseValidatorRewrite ) info( pos, s, force = false )

  private val Function( prototype, vimpl ) = v.tree
  if ( prototype.size != 1 )
    abort( prototype.tail.head.pos, "Only single-parameter validators are supported!" )

  private case class Subvalidator( description: String, extractor: Tree, validation: Tree, ouvtpe: Type )

  private val validatorType = typeOf[ Validator[_] ]
  private val function1Type = typeOf[ Function1[_,_] ]
  private val contextualizerTerm = typeOf[ builder.Contextualizer[_] ].typeSymbol.name.toTermName


  private object ValidatorApplication {
    def extractObjectUnderValidation( t: Tree ) =
      extractFromPattern( t ) {
        case Apply( TypeApply( Select( _, `contextualizerTerm` ), tpe :: Nil ), e :: Nil ) =>
          ( context.resetAllAttrs( e.duplicate ), tpe.tpe )
      } getOrElse
        abort( t.pos, s"Failed to extract object under validation from tree $t (raw=${showRaw(t)})" )

    def rewriteContextExpressionAsValidator( expr: Tree, extractor: Tree ) =
      context.resetAllAttrs( transformByPattern( expr ) {
        case Apply( t @ TypeApply( Select( _, `contextualizerTerm` ), _ ), e :: Nil ) =>
          Apply( t, extractor :: Nil )
      } )

    def unapply( expr: Tree ): Option[ Subvalidator ] = expr match {
      case t if t.tpe <:< validatorType =>
        val ( ouv, ouvtpe ) = extractObjectUnderValidation( expr )
        val extractor = Function( prototype, ouv )
        val sv = rewriteContextExpressionAsValidator( expr, ouv )
        val para = prototype.head.name
        val desc = ouv match {
          case Select( Ident( `para` ), selector ) => selector.toString()
          case _ => ouv.toString()
        }
        log( s"""
              |Found subvalidator:
              |  ouv=$ouv
              |  ouvraw=${showRaw(ouv)}
              |  ouvtpe=$ouvtpe
              |  extractor=${show(extractor)}
              |  extractorraw=${showRaw(extractor)}
              |  sv=${show(sv)}
              |  svraw=${showRaw(sv)}
              |  desc=$desc
              |""".stripMargin, ouv.pos )
        Some( Subvalidator( desc, extractor, sv, ouvtpe ) )

      case _ => None
    }
  }

  private def findSubvalidators( t: Tree ): List[ Subvalidator ] = t match {
    case Block( stats, expr ) => ( stats flatMap findSubvalidators ) ++ findSubvalidators( expr )
    case ValidatorApplication( validator ) => validator :: Nil
    case Literal( Constant(()) ) => Nil   // Ignored terminator
    case _ => abort( t.pos, s"Unexpected node $t:\n\ttpe=${t.tpe}\n\traw=${showRaw(t)}" )
  }

  // Rewrite expressions into a validation chain --

  /**
    * Each subvalidator of type Validator[ U ] is essentially rewritten as Validator[ T ], similar to:
    *
    * ```
    * val rewriteOne( description: String, extractor: T => U, sv: Validator[ U ] ): Validator[ T ] =
    *   ( value: T ) => {
    *     sv( extractor( value ) ) match {
    *       case Success => Success
    *       case Failure( violations ) => Failure( violations map prefixWith( description ) )
    *   }
    * ```
    *
    * Due to Scala macro limitations, in practice the Validator[ T ] is implemented as an anonymous class of type
    * Function1[ T, Result ], which is instantiated and returned as the expression value.
    *
    * @param sv The subvalidator to rewrite
    * @return A valid expression representing a [[com.tomergabel.util.validation.Validator]] of `T`.
    */
  private def rewriteOne( sv: Subvalidator ): Expr[ Validator[ T ] ] = {

    // Export the description as a string literal to be spliced in later
    val descExpr = context.Expr[ String ]( Literal( Constant( sv.description ) ) )

    // Define the apply() function body (recall that we're in practice implementing Function1[ T, Result ])
    val applydef = {
      val Function( _, extractorImpl ) = sv.extractor   // TODO extractor as a function probably unnecessary. Clean up
      val svdef = ValDef( NoMods, newTermName( "sv" ), TypeTree(), sv.validation )
      val applysel = Apply( Ident( svdef.name ), extractorImpl :: Nil )

      val successCase = CaseDef( Ident( newTermName( "Success" ) ), EmptyTree, Ident( newTermName( "Success" ) ) )
      val failCase = {
        val vterm = newTermName( "violations" )
        val vexpr = context.Expr[ Seq[ Violation ] ]( Ident( vterm ) )
        val vappl =
          reify { Failure( vexpr.splice map { f => f.copy( constraint = descExpr.splice + " " + f.constraint ) } ) }
        CaseDef(
          Bind( newTermName( "f" ), Apply( Ident( newTermName( "Failure" ) ), List( Bind( vterm, Ident( nme.WILDCARD ) ) ) ) ),
          EmptyTree,
          vappl.tree
        )
      }

      val applyimpl = Block(
        svdef :: Nil,
        Match( applysel, successCase :: failCase :: Nil )
      )

      DefDef( NoMods, newTermName( "apply" ), Nil, List( prototype ), TypeTree(), applyimpl )
    }

    // Declare the anonymous class and wrapper block
    val anon = newTypeName( context.fresh() )
    val vtype = TypeTree( appliedType( validatorType.typeConstructor, weakTypeOf[ T ] :: Nil ) )
    val cdef = ClassDef( NoMods, anon, Nil, Template( vtype :: Nil, emptyValDef, defaultCtor() :: applydef :: Nil ) )
    val ctor = Apply( Select( New( Ident( anon ) ), nme.CONSTRUCTOR ), List.empty )
    val rewrite = context.Expr[ Validator[ T ] ]( Block( cdef :: Nil, ctor ) )

    // Report and return the rewritten validator
    log( s"""|Subvalidator:
             |  Description: ${sv.description}
             |  Extractor  : ${sv.extractor}
             |  Validation : ${sv.validation}
             |
             |Rewritten as:
             |  Clean      : ${show( rewrite )}
             |  Raw        : ${showRaw( rewrite )}
             |""".stripMargin, sv.validation.pos )
    rewrite
  }

  /** Returns the specified validation block, transformed into a single monolithic validator.
    *
    * @return The transformed [[com.tomergabel.util.validation.Validator]] of `T`.
    */
  def transformed: Expr[ Validator[ T ] ] = {
    // Rewrite all validators
    val subvalidators = findSubvalidators( vimpl ) map rewriteOne
    val svseq: Expr[ Seq[ Validator[ T ] ] ] = subvalidators.consolidate
    val result: Expr[ Validator[ T ] ] = reify { new combinators.And( svseq.splice :_* ) }

    log( s"""|Result of validation transform:
             |  Clean: ${show( result )}
             |  Raw  : ${showRaw( result )}
             |""".stripMargin )
    result
  }
}

object ValidationTransform {
  def apply[ T : c.WeakTypeTag ]( c: Context )( v: c.Expr[ T => Unit ] ): c.Expr[ Validator[ T ] ] =
    new ValidationTransform[ c.type, T ]( c, v ).transformed
}