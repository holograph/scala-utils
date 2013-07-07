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

   Created by tomer on 2/3/13.
 */

package com.tomergabel.util.profiler

import scala.collection.mutable

/**
 * Millisecond-resolution internal profiler. Can (should?) be optimized to have near-zero overhead in the common
 * case, although this will most likely be fast enough for most use-cases.
 *
 * The Profiler trait enables the following on the call site:
 *
 * {{{
 * abstract class MyClass extends Profiler {
 *     def someMethod() {
 *         profile( "root" ) {
 *         profile( "call1" ) { Thread.sleep( 1500 ) }
 *         profile( "call2" ) {
 *             profile( "calla" ) { Thread.sleep( 100 ) }
 *             profile( "callb" ) { Thread.sleep( 200 ) }
 *         }
 *         profile( "call3" ) { Thread.sleep( 200 ) }
 *    }
 * }
 * }}}
 *
 * The output will consequently look like this:
 * {{{
 * root (2000ms)
 *  |- call1 (1500ms)
 *  |- call2 (300ms)
 *  |    |- calla (100ms)
 *  |    |- callb (200ms)
 *  |- call3 (200ms
 * }}}
 *
 * Profiler is abstract, leaving the details of toggling, rendering and output to the user. There are a
 * number of implementations available out of the box, including NullProfiler and ConsoleProfiler.
 */
trait Profiler {
  import Profiler._

  protected def isProfilerEnabled: Boolean
  protected def logProfilerResult( root: ProfilerFrame )

  protected def profile[ R ]( section: String )( x: => R ): R =
    if ( !isProfilerEnabled ) x
    else {
      val frame = new ProfilerFrame( section )
      push( frame )
      try x
      finally {
        frame.mark()
        pop() match {
          case Some( parent ) => parent aggregate frame
          case None => logProfilerResult( frame )
        }
      }
    }

  protected def renderProfilerResult( root: ProfilerFrame ) = Profiler.renderResult( root )
}

object Profiler {
  // Stack management
  private[ profiler ] val _activeStack = new ThreadLocal[ mutable.Stack[ ProfilerFrame ] ]()
  private[ profiler ] def push( frame: ProfilerFrame ) {
    val stack =
      Option( _activeStack.get )
        .getOrElse { val n = new mutable.Stack[ ProfilerFrame ](); _activeStack.set( n ); n }
    stack.push( frame )
  }
  private[ profiler ] def pop(): Option[ ProfilerFrame ] = {
    val stack =
      Option( _activeStack.get )
        .getOrElse { throw new IllegalStateException( "Pop called on an empty profiler stack?" ) }
    stack.pop()
    stack.headOption
  }

  def renderResult( root: ProfilerFrame ) = {
    val buffer = new mutable.StringBuilder( 200 )   // TODO tune?

    /*
        Desired sample output:
        root (20000ms)
          |- call1 (15000ms)
          |- call2 (3000ms)
          |    |- calla (1000ms)
          |    |- callb (2000ms)
          |- call3 (2000ms)
    */
    def render( frame: ProfilerFrame, indent: Int ) {
      for ( _ <- 0 until indent - 1 ) buffer append "  |  "
      if ( indent > 0 ) buffer append "  |- "
      buffer append frame.name append " (" append frame.elapsed append "ms)\n"
      frame.children foreach { render( _, indent + 1 ) }
    }
    render( root, 0 )
    buffer.toString()
  }
}
