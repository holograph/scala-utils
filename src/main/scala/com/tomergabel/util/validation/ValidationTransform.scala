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
    }.transform( tree )

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
        case Apply( TypeApply( Select( _, `contextualizerTerm` ), tpe :: Nil ), e :: Nil ) => ( e, tpe.tpe )
      } getOrElse
        abort( t.pos, s"Failed to extract object under validation from tree $t (raw=${showRaw(t)})" )

    def rewriteContextExpressionAsValidator( expr: Tree, extractor: Tree ) =
      transformByPattern( expr ) {
        case Apply( t @ TypeApply( Select( _, `contextualizerTerm` ), _ ), e :: Nil ) =>
          Apply( t, extractor :: Nil )
      }

    def unapply( expr: Tree ): Option[ Subvalidator ] = expr match {
      case t if t.tpe <:< validatorType =>
        val ( ouv, ouvtpe ) = extractObjectUnderValidation( expr )
        val extractor = Function( prototype, t )
        val sv = rewriteContextExpressionAsValidator( expr, ouv )
        info( ouv.pos, s"""
              |Found subvalidator:
              |  ouv=$ouv
              |  ouvraw=${showRaw(ouv)}
              |  ouvtpe=$ouvtpe
              |  extractor=${show(extractor)}
              |  extractorraw=${showRaw(extractor)}
              |  sv=${show(sv)}
              |  svraw=${showRaw(sv)}
              |""".stripMargin, force = false )
        Some( Subvalidator( ouv.toString(), extractor, sv, ouvtpe ) )

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

  private def rewriteOne( sv: Subvalidator ): Expr[ Validator[ T ] ] = {
    val descExpr = context.Expr[ String ]( Literal( Constant( sv.description ) ) )

    val applydef = {
      val Function( _, extractorImpl ) = sv.extractor
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
        Ident( newTermName( "Success" ) )//Match( applysel, successCase :: failCase :: Nil )
      )

      DefDef( NoMods, newTermName( "apply" ), Nil,
        List( prototype ),
        TypeTree(), applyimpl )
    }

    val anon = newTypeName( context.fresh() )
    val cdef = ClassDef( NoMods, anon, Nil,
      Template( TypeTree( appliedType( validatorType.typeConstructor, weakTypeOf[ T ] :: Nil ) ) :: Nil, emptyValDef, defaultCtor() :: applydef :: Nil ) )
    val ctor = Apply( Select( New( Ident( anon ) ), nme.CONSTRUCTOR ), List.empty )

    val rewrite = context.Expr[ Validator[ T ] ]( Block( cdef :: Nil, ctor ) )

    info( sv.validation.pos,
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

  def transformed: Expr[ Validator[ T ] ] = {
    val subvalidators = findSubvalidators( vimpl )
    val rewritten: List[ Expr[ Validator[ T ] ] ] = subvalidators map rewriteOne

    //      val validators: Expr[ Seq[ Validator[ T ] ] ] = c.Expr[ Seq[ Validator[ T ] ] ](
    //        Apply( Select( Ident( newTermName( "Seq" ) ), newTermName( "apply" ) ), rewritten map { _.tree } )
    //      )
    //
    //      val result: Expr[ Validator[ T ] ] = reify { new And( validators.splice :_* ) }

    val result = rewritten.head

    info( vimpl.pos,
      s"""|Result:
            |  Clean: ${show( result )}
            |  Raw  : ${showRaw( result )}
            |""".stripMargin, force = false )

    result
  }
}

object ValidationTransform {
  def apply[ T : c.WeakTypeTag ]( c: Context )( v: c.Expr[ T => Unit ] ): c.Expr[ Validator[ T ] ] =
    new ValidationTransform[ c.type, T ]( c, v ).transformed
}