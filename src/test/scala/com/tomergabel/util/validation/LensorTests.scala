package com.tomergabel.util.validation


import LensorTests._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec

class LensorTests extends WordSpec with ShouldMatchers {

  "lensorOf[ TestClass ]" should {
    val tc = TestClass( "ham", 42, NestedClass( "eggs" ) )

    "provide access to a primitive field via accessor" in {
      val lensor = Lensor.lensorOf[ TestClass ]
      lensor.primitive( tc ) should be === 42
    }
    "provide access to a reference field via accessor" in {
      val lensor = Lensor.lensorOf[ TestClass ]
      lensor.reference( tc ) should be === "ham"
    }
    "provide access to an arbitrarily-typed field via accessor" in {
      val lensor = Lensor.lensorOf[ TestClass ]
      lensor.arbitrary( tc ) should be === NestedClass( "eggs" )
    }

    // TODO test generic return values
  }
}

object LensorTests {
  case class NestedClass( x: String )
  case class TestClass( reference: String, primitive: Int, arbitrary: NestedClass )
}