package com.tomergabel.util.metrics

import java.lang.Thread
import scala.util.Random
import java.nio.channels.DatagramChannel
import java.nio.ByteBuffer
import actors.threadpool.{TimeoutException, TimeUnit, Callable, Executors}
import org.scalatest.matchers.{MatchResult, BeMatcher, ShouldMatchers}
import org.scalatest.{Suites, fixture}

/**
 * Created by tomer on 10/10/11.
 */

import StatsdClientTests._

class StatsdClientTests extends Suites( new ServerlessTests, new ServerTests )

private class ServerlessTests extends org.scalatest.FlatSpec with ShouldMatchers {
  val client = new StatsdClient()
  import client._

  // Warmup (needed to perform classloading etc., otherwise the times are thrown off)
  try {
    counter( "test" ).increment()
    counter( "test" ).decrement()
    counter( "test" ).add( 5 )
    counter( "test" ).subtract( 5 )
    counter( "test", Some( 0.1f ) ).increment()
    counter( "test", Some( 0.1f ) ).decrement()
    counter( "test", Some( 0.1f ) ).add( 5 )
    counter( "test", Some( 0.1f ) ).subtract( 5 )
    emitTime( "test", 20 )
    time( "test" ) {}
  } catch { case e: Exception => }

  val tolerance = 500000L     // 0.5ms
  def timed( x: => Unit ) = { val start = System.nanoTime(); x; System.nanoTime() - start }

  "StatsdClient.counter" should "not throw an exception on increment" in {
    counter( "test" ).increment()
  }
  it should "take negligible time on increment" in {
    timed { counter( "test" ).increment() } should be <= tolerance
  }
  it should "not throw an exception on decrement" in {
    counter( "test" ).decrement()
  }
  it should "take negligible time on decrement" in {
    timed { counter( "test" ).decrement() } should be <= tolerance
  }
  it should "not throw an exception on add" in {
    counter( "test" ).add( 5 )
  }
  it should "take negligible time on add" in {
    timed { counter( "test" ).add( 5 ) } should be <= tolerance
  }
  it should "not throw an exception on subtract" in {
    counter( "test" ).subtract( 5 )
  }
  it should "take negligible time on subtract" in {
    timed { counter( "test" ).subtract( 5 ) } should be <= tolerance
  }

  "StatsdClient.emitTime" should "not throw an exception" in {
    emitTime( "test", 20 )
  }
  it should "take negligible time" in {
    timed { emitTime( "test", 20 ) } should be <= tolerance
  }

  "StatsdClient.time" should "not throw an exception" in {
    time( "test" ) {}
  }
  it should "take negligible time" in {
    timed {
      time( "test" ) {}
    } should be <= tolerance
  }
}

private class ServerTests extends StatsdClientTestSuite with ShouldMatchers {
  "StatsdClient.counter" should "issue correct command on increment" in { listener => import listener._
    listen { counter( "test.ham" ).increment() } should be === "test.ham:1|c"
  }
  it should "issue correct command on decrement" in { listener => import listener._
    listen { counter( "test.eggs" ).decrement() } should be === "test.eggs:-1|c"
  }
  it should "issue correct command on add" in { listener => import listener._
    listen { counter( "test.eggs" ).add( 5 ) } should be === "test.eggs:5|c"
  }
  it should "issue correct command on subtract" in { listener => import listener._
    listen { counter( "test.eggs" ).subtract( 5 ) } should be === "test.eggs:-5|c"
  }

  // TODO add sample rate tests --TG

  "StatsdClient.emitTime" should "issue the correct command" in { listener => import listener._
    listen { emitTime( "test.gnarly", 20 ) } should be === "test.gnarly:20|ms"
  }

  "StatsdClient.time" should "measure " + halfTime + "ms and issue correct command" in { listener => import listener._
    listen {
      time( "test.foo" ) { Thread.sleep( halfTime ) }
    } should be( timed( "test.foo", halfTime ) )
  }
  it should "ignore errors on a timed section by default, and not issue a command" in { listener => import listener._
    expectNothing {
      evaluating {
        time( "test.bar" ) {
          Thread.sleep( 200L )
          throw new Exception()
        }
      } should produce[ Exception ]
    }
  }
  it should "measure " + halfTime + "ms and issue correct command when logOnFail=true" in { listener => import listener._
    listen {
      evaluating {
        time( "test.baz", logOnFail = true ) {
          Thread.sleep( halfTime )
          throw new Exception()
        }
      } should produce[ Exception ]
    } should be( timed( "test.baz", halfTime ) )
  }
}

object StatsdClientTests {
  val portRange = Range( 10000, 20000 )
  val charset = java.nio.charset.Charset.defaultCharset()
  val testTimeout = 100L
  val halfTime = testTimeout / 2
  val worker = Executors.newSingleThreadExecutor()

  import java.net._
  private def randomizeSocket() = {
    val channel = DatagramChannel.open()
    while ( !channel.socket.isBound ) {
      val port = Random.nextInt( portRange.length ) + portRange.start
      try channel.socket().bind( new InetSocketAddress( "localhost", port ) )
      catch { case e: SocketException => }
    }
    channel
  }

  class TestHarness extends MetricOps {
    private val channel = randomizeSocket()
    def port = channel.socket().getLocalPort
    private val buffer = ByteBuffer.allocate( 4096 )
    private val client = new StatsdClient( "localhost", port )

    def listen( x: => Unit ) = {
      val future = worker.submit( new Callable {
        def call() = {
          buffer.clear()
          channel.receive( buffer )
          buffer.flip()
          val message = charset.decode( buffer ).toString
          message
        }
      } )
      x
      future.get( testTimeout, TimeUnit.MILLISECONDS ).asInstanceOf[ String ]
    }

    def expectNothing( x: => Unit ) {
      import ShouldMatchers._
      evaluating { this.listen( x ) } should produce [ TimeoutException ]
    }

    def close() {
      client.close()
      channel.close()
    }

    def counter( key: String, sampleRate: Option[ Float ] = None ) = client.counter( key, sampleRate )
    def time[ T ]( key: String, logOnFail: Boolean = false )( x: => T ) = { client.time( key, logOnFail )( x ) }
    def emitTime( key: String, timeInMillis: Long ) { client.emitTime( key, timeInMillis ) }
  }

  // Some syntactic hacks to make the tests cleaner :-) --TG

  trait StatsdClientTestSuite extends fixture.FlatSpec {
    type FixtureParam = TestHarness
    protected def withFixture( test: OneArgTest ) {
      val listener = new TestHarness
      try test( listener )
      finally {
        listener.close()
      }
    }
  }

  val timerRegex = """(.+):([0-9.]+)\|ms""".r
  val timerTolerance = 1L    //1L
  case class timed( key: String, howLong: Long ) extends BeMatcher[ String ] {
    def apply( left: String ) = {
      timerRegex.findFirstMatchIn( left ) match {
        case None =>
          MatchResult( matches = false, failureMessage = "Not a timer: " + left, negatedFailureMessage = "A timer: " + left )
        case Some( m ) if m.group( 1 ) != key =>
          MatchResult( matches = false, failureMessage = "Incorrect timed section key: " + left, negatedFailureMessage = "Correct timed section key: " + left )
        case Some( m ) if m.group( 2 ).toInt < ( howLong - timerTolerance ) || m.group( 2 ).toInt > ( howLong + timerTolerance ) =>
          MatchResult( matches = false, failureMessage = "Timed section outside allowed tolerance: " + left, negatedFailureMessage = "Timed section within allowed tolerance: " + left )
        case _ => MatchResult( matches = true, failureMessage = "", negatedFailureMessage = "" )
      }
    }
  }
}