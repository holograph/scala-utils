package com.tomergabel.util.profiler.impl

import com.tomergabel.util.profiler.{ProfilerFrame, Profiler}

/**
 * Created by tomer on 2/4/13.
 */
trait NullProfiler extends Profiler {
    protected def isProfilerEnabled = false
    protected def logProfilerResult( root: ProfilerFrame ) {}
}
