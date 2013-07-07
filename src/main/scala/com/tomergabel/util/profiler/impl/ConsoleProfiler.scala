package com.tomergabel.util.profiler.impl

import com.tomergabel.util.profiler.{ProfilerFrame, Profiler}

/**
 * Created by tomer on 2/4/13.
 */
trait ConsoleProfiler extends Profiler {
  protected def isProfilerEnabled = true
  protected def profilerOutput = System.err
  protected def logProfilerResult( root: ProfilerFrame ) {
    profilerOutput.println( renderProfilerResult( root ) )
  }
}
