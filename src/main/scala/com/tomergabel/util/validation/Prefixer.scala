package com.tomergabel.util.validation

import scala.language.experimental.macros
import scala.reflect.macros._

/**
 * Created by tomer on 8/22/13.
 */
/*private[ validation ]*/ object Prefixer {
  class Wrapper[ T ]( property: String, validator: Validator[ T ] ) extends Validator[ T ] {
    def apply( v: T ) = validator( v ) match {
      case Success => Success
      case Failure( violations ) =>
        Failure( violations map { f => f.copy( constraint = property + " " + f.constraint ) } )
    }
  }

  def prefixer_impl[ T : c.WeakTypeTag ]( c: Context )( v: c.Expr[ Validator[ T ] ] ): c.Expr[ Validator[ T ] ] = {
    import c.universe._
    val Function( List(_), impl ) = v.tree
    val Apply( Select( Apply( TypeApply( _, _ ), selector @ List(_) ), _ ), validations @ List(_) ) = impl
    val List( Select( _, property ) ) = selector

    val pname = c.Expr[ String ]( Literal( Constant( property.decoded ) ) )   // Probably a cleaner way to do this... *sigh*
    reify { new Prefixer.Wrapper( pname.splice, v.splice ) }

//    println( showRaw( impl ) )
//    v
  }

  def prefix[ T ]( v: Validator[ T ] ): Validator[ T ] = macro prefixer_impl[ T ]
}

//      Apply(
//        Select(
//          Apply(
//            TypeApply(
//              Select(
//                Select(
//                  Select(
//                    This(newTypeName("validation")),
//                    com.tomergabel.util.validation.package
//                  ),
//                  com.tomergabel.util.validation.builder
//                ),
//                newTermName("Contextualizer")
//              ),
//              List(TypeTree())
//            ),
//            List(
//              Select(Ident(newTermName("x$1")), newTermName("firstName"))
//            )
//          ),
//          newTermName("is")
//        ),
//        List(TypeApply(Select(Select(Select(This(newTypeName("validation")), com.tomergabel.util.validation.package), com.tomergabel.util.validation.builder), newTermName("notEmpty")), List(TypeTree())))
//      )
