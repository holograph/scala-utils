package com.tomergabel.util.metrics

/**
 * Created by tomer on 10/11/11.
 */

trait Counter {
  def add( delta: Int )
  def subtract( delta: Int )
  @inline def increment() { add( 1 ) }
  @inline def decrement() { subtract( 1 ) }
}

trait MetricOps {
  def counter( key: String, sampleRate: Option[ Float ] = None ): Counter
  def time[ T ]( key: String, logOnFail: Boolean = false )( x: => T ): T
  def emitTime( key: String, timeInMillis: Long )
}

private object NullCounter extends Counter {
  def add( delta: Int ) {}
  def subtract( delta: Int ) {}
}

trait NullMetricOps extends MetricOps {
  @inline def counter( key: String, sampleRate: Option[ Float ] = None ): Counter = NullCounter
  @inline def time[T]( key: String, logOnFail: Boolean = false )( x: => T ) = { x }
  @inline def emitTime( key: String, timeInMillis: Long ) {}
}

// Note: Counters with dots in them will resolve to a folder hierarchy in Graphite
trait Metrics extends MetricOps {
  import Metrics._
  @inline def counter( key: String, sampleRate: Option[ Float ] = None ): Counter = client.counter( key, sampleRate )
  @inline def time[T]( key: String, logOnFail: Boolean = false )( x: => T ): T = { client.time( key, logOnFail )( x ) }
  @inline def emitTime( key: String, timeInMillis: Long ) { client.emitTime( key, timeInMillis ) }
}

object Metrics extends Metrics {
  val client = new StatsdClient()     // localhost:8125 assumed by convention
}
