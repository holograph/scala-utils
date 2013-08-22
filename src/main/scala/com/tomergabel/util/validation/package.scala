package com.tomergabel.util

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
  }

  object builder {
    import combinators._

    def validator[ T ]( v: Validator[ T ]* ): Validator[ T ] = new And( v:_* )

//    def validator[ T ]( v: Validator[ T ]* ): Validator[ T ] = new And( ( v map Prefixer.prefix ):_* )

    implicit class Contextualizer[ U ]( value: U ) {
      def is( validator: Validator[ U ] ) = validator( value )
      def has( validator: Validator[ U ] ) = validator( value )
    }
    implicit class ExtendValidator[ T ]( validator: Validator[ T ] ) {
      def and( other: Validator[ T ] ) = new And( validator, other ) // TODO shortcut multiple ANDs
      def or( other: Validator[ T ] ) = new Or( validator, other )   // TODO shortcut multiple ORs
    }

    def empty[ T <: HasEmpty ] = new Empty[ T ]
    def notEmpty[ T <: HasEmpty ] = new NotEmpty[ T ]
    def size[ T <: HasSize ] = new Size[ T ]
  }
}
