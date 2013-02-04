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

/**
 * Created by tomer on 2/3/13.
 */
class ProfilerFrame( val name: String ) {
    private var _end: Option[ Long ] = None
    private var _children: Seq[ ProfilerFrame ] = List.empty
    val start = System.currentTimeMillis()

    def end = _end getOrElse { throw new IllegalStateException( "End called on unmarked profiler frame?" ) }
    def elapsed = end - start
    def children = _children

    private[ profiler ] def aggregate( child: ProfilerFrame* ) { _children ++= child }
    private[ profiler ] def mark() { _end = Some( System.currentTimeMillis() ) }
}
