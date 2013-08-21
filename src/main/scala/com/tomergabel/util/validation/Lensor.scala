package com.tomergabel.util.validation

import scala.language.experimental.macros
import scala.reflect.macros.Context

/**
 * An experimental macro-based transformer. lensorOf[ T ] takes a class T and produces a transformed class U, so
 * that for each accessor of the form `def acc: R` a corresponding extractor is generated in U of the form
 * `def acc: T => R`.
 */
object Lensor {
  def lensorOf[ T ]: Any = macro lensorOf_impl[ T ]

  def lensorOf_impl[ T : c.WeakTypeTag ]( c: Context ) = {
    import c.universe._

    val f1tc = typeOf[ Function1[ _, _ ] ].typeConstructor  // There's probably a cleaner way to do this
    val typeOfT = weakTypeOf[ T ]
    val accessors = typeOfT.members.collect { case m if m.isMethod && m.asMethod.isAccessor => m.asMethod }
    val processedAccessors = accessors.map { acc =>
      ValDef(
        NoMods,
        newTermName( acc.name.decoded ),
        TypeTree( c.universe.appliedType( f1tc, List( typeOfT, acc.returnType ) ) ), {
          val v = newTermName( "v" )
          Function( ValDef( NoMods, v, TypeTree(), EmptyTree ) :: Nil, Select( Ident( v ), acc.name ) )
        }
      )
    }.toList

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

    val lensor =
        ClassDef(
          Modifiers(),
          newTypeName( c.fresh() ),
          Nil,
          Template(
            Ident( typeOf[ AnyRef ].typeSymbol ) :: Nil,
            emptyValDef,
            defaultCtor() :: processedAccessors
          )
        )

    // A final wrapper (though anonymous) class is needed to expose structural types per:
    // http://stackoverflow.com/questions/14370842/getting-a-structural-type-with-an-anonymous-classs-methods-from-a-macro
    val wrapper = newTypeName( c.fresh() )
    c.Expr(
      Block(
        lensor ::
          ClassDef(
            Modifiers( Flag.FINAL ),
            wrapper,
            Nil,
            Template(
              Ident( lensor.name ) :: Nil,
              emptyValDef,
              defaultCtor() :: Nil
            )
          ) :: Nil,
        Apply( Select( New( Ident( wrapper ) ), nme.CONSTRUCTOR ), Nil )
      )
    )
  }
}
