/*
   Copyright 2013 Tomer Gabel

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

package com.tomergabel.util.profiler

import org.scalatest.matchers.{MatchResult, Matcher, ShouldMatchers}
import org.scalatest.FlatSpec

/**
 * Created by tomer on 2/3/13.
 */
class ProfilerTests extends FlatSpec with ShouldMatchers {

    class UnderTest extends Profiler {
        protected def isProfilerEnabled = true
        protected def logProfilerResult( root: ProfilerFrame ) {
            result = Some( root )
        }

        var result: Option[ ProfilerFrame ] = None
        def rendered = renderProfilerResult( result.get )

        def m() {
            profile( "root" ) {
                profile( "call1" ) { Thread.sleep( 1500 ) }
                profile( "call2" ) {
                    profile( "calla" ) { Thread.sleep( 100 ) }
                    profile( "callb" ) { Thread.sleep( 200 ) }
                }
                profile( "call3" ) { Thread.sleep( 200 ) }
            }
        }
    }

    val tolerance = 50L
    def matchFrame( name: String, time: Long ) = new Matcher[ ProfilerFrame ] {
        def apply( left: ProfilerFrame ) =
            if ( left.name != name )
                new MatchResult( false,
                    "Profiler stack frame has incorrect name %s, expected %s".format( left.name, name ),
                    "Profiler stack frame has unexpected name %s".format( name ) )
            else if ( left.elapsed < time - tolerance || left.elapsed > time + tolerance )
                new MatchResult( false,
                    "Profiler stack frame (%dms long) does not match expected time frame ~%dms".format( left.elapsed, time ),
                    "Profiler stack frame (%dms long) should not match expected time frame ~%dms".format( left.elapsed, time ) )
            else
                new MatchResult( true, "matches", "does not match" )
    }

    "profile" should "yield a root profiler frame when call tree is complete" in {
        val test = new UnderTest
        test.m()
        test.result.isDefined should be === true
        test.result.get should matchFrame( "root", 2000 )
        test.result.get.children match {
            case c1 :: c2 :: c3 :: Nil =>
                c1 should matchFrame( "call1", 1500 )
                c2 should matchFrame( "call2", 300 )
                c2.children match {
                    case ca :: cb :: Nil =>
                        ca should matchFrame( "calla", 100 )
                        cb should matchFrame( "callb", 200 )
                    case _ =>
                        fail( "call2 should contain child frames!" )
                }
                c3 should matchFrame( "call3", 200 )
            case _ => fail( "Root should contain child frames!" )
        }
    }

    "renderResult" should "correctly render a profiler stack" in {
        val expectedRenderedOutput =
            """
              |root (2000ms)
              |  |- call1 (1500ms)
              |  |- call2 (300ms)
              |  |    |- calla (100ms)
              |  |    |- callb (200ms)
              |  |- call3 (200ms)
            """.stripMargin.trim

        val root  = new ProfilerFrame( "root"  ) { override def elapsed = 2000L }
        val call1 = new ProfilerFrame( "call1" ) { override def elapsed = 1500L }
        val call2 = new ProfilerFrame( "call2" ) { override def elapsed =  300L }
        val calla = new ProfilerFrame( "calla" ) { override def elapsed =  100L }
        val callb = new ProfilerFrame( "callb" ) { override def elapsed =  200L }
        val call3 = new ProfilerFrame( "call3" ) { override def elapsed =  200L }
        root.aggregate( call1, call2, call3 )
        call2.aggregate( calla, callb )

        val result = Profiler.renderResult( root ).trim
        result should be === expectedRenderedOutput
    }
}
