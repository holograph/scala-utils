Overview
========

An inline profiler designed to provide a rudimentary, zero-overhead facility for diagnosing performance in Scala code.

This module is part of [scala-utils](/README.md).

Usage
=====

At the call site
----------------

The `Profiler` trait provides a simple API to define a profiled scope, or a profiler _frame_. These frames can be freely nested to create an arbitrary profile tree. To illustrate, the following code:

```scala
            profile( "root" ) {
                profile( "call1" ) { Thread.sleep( 1500 ) }
                profile( "call2" ) {
                    profile( "calla" ) { Thread.sleep( 100 ) }
                    profile( "callb" ) { Thread.sleep( 200 ) }
                }
                profile( "call3" ) { Thread.sleep( 200 ) }
            }
```
Will generate roughly the following output:

```
root (2003ms)
  |- call1 (1501ms)
  |- call2 (301ms)
  |    |- calla (101ms)
  |    |- callb (200ms)
  |- call3 (201ms)
```

Output
------

The `Profiler` trait is abstract and is complemented by output implementations which reside in the `com.tomergabel.util.profiler.impl` package. Currently the two available implementations are:
* `NullProfiler` which can be used to mock the profiler for test purposes; and
* `ConsoleProfiler` which does as the name implies, although it can be overriden to write to any `PrintStream`.

To provide your own concrete profiler, you must implement the two `Profiler` methods:
```scala
    protected def isProfilerEnabled: Boolean
    protected def logProfilerResult( root: ProfilerFrame )
```
You can render a text representation of of the profile tree using `Profiler.renderProfilerResult`.

Rendering
---------
You can control the default profiler rendering (as seen above) by overriding `Profiler.renderProfilerResult`. This can be used to e.g. generate a structured JSON stream that you can feed to your monitoring system.

License 
=======

All code in this repository is shared under the [Apache Software License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).

~         