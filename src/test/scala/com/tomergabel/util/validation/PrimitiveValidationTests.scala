//package com.tomergabel.util.validation
//
//import org.scalatest.WordSpec
//import org.scalatest.matchers.ShouldMatchers
//
///**
// * Created by tomer on 8/7/13.
// */
//
//object PrimitiveValidationTests {
//  import builder._
//
//  case class Person( firstName: String, lastName: String )
//  case class Classroom( teacher: Person, students: Seq[ Person ] )
//
//  implicit val personValidator = validator[ Person ](
//    _.firstName is notEmpty,
//    _.lastName is notEmpty
//  )
//
//  implicit val classValidator = validator[ Classroom ]( _.students has size > 0 )
//}
//
//import PrimitiveValidationTests._
//class PrimitiveValidationTests extends WordSpec with ShouldMatchers {
//
//  val personWithNoName = Person( "", "" )
//  val personWithNoFirstName = Person( "", "last" )
//  val personWithNoLastName = Person( "first", "" )
//  val legitPerson1 = Person( "first", "person" )
//  val legitPerson2 = Person( "second", "person" )
//  val legitPerson3 = Person( "third", "dude" )
//  val classWithIllegalTeacher = Classroom( personWithNoName, Seq( legitPerson1, legitPerson2, legitPerson3 ) )
//  val classWithNoStudents = Classroom( legitPerson1, Seq.empty )
//  val classWithIllegalStudent = Classroom( legitPerson1, Seq( legitPerson2, personWithNoLastName ) )
//
//  def failWith( expectedViolations: String* ) = new Matcher[ Result ] {
//    def apply[ S <: Result ]( t: Expectable[ S ] ): MatchResult[ S ] =
//      t.value match {
//        case Success => result( test = false, "Validation was successful", "Validation was not successful", t )
//        case Failure( vlist ) =>
//          val violations = vlist.map { _.constraint }.toSet
//          val remainder = expectedViolations.toSet -- violations
//          val unexpected = violations.toSet -- expectedViolations.toSet
//          result( remainder.isEmpty && unexpected.isEmpty,
//            "Validation failed with unexpected violations!\nExpected violations that weren't found:\n"
//              + remainder.mkString( "\t", "\n\t", "\n" )
//              + "Unexpected violations:\n" + unexpected.mkString( "\t", "\n\t", "\n" ),
//            // How to negate?
//            "Validation failed with unexpected violations!\nExpected violations that weren't found:\n"
//              + remainder.mkString( "\t", "\n\t", "\n" )
//              + "Unexpected violations:\n" + unexpected.mkString( "\t", "\n\t", "\n" ),
//            t )
//      }
//  }
//
//  "personValidator" should {
//    "fail a person with no first name" in {
//      val result = validate( personWithNoFirstName )
//      result should failWith( "Must not be empty" )
//    }
//    "fail a person with no last name" in {
//      val result = validate( personWithNoLastName )
//      result should beAnInstanceOf[ Failure ]
//    }
//    "pass a person with a full name" in {
//      val result = validate( legitPerson1 )
//      result should be equalTo Success
//    }
//    "fail a classroom with no students" in {
//      val result = validate( classWithNoStudents )
//      result should failWith( "Got 0, expected more than 0" )
//    }
//    "fail a classroom with an invalid teacher" in pending
//    "fail a classroom with an invalid student" in pending
//
//    // TOOD "explanation" tests
//  }
//}
