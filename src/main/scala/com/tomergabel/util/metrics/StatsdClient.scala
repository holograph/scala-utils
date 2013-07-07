package com.tomergabel.util.metrics

import java.nio.channels.DatagramChannel
import java.net.{PortUnreachableException, InetSocketAddress}

/**
 * Created by tomer on 10/10/11.
 */

class StatsdClient( val host: String = "localhost", val port: Int = 8125 ) extends MetricOps {
  private val address = new InetSocketAddress( host, port )
  private var _channel = DatagramChannel.open().connect( address )
  private def channel = {
    if ( !_channel.isOpen ) _channel = DatagramChannel.open().connect( address )
    if ( !_channel.isConnected ) _channel.connect( address )
    _channel
  }

  private val charset = java.nio.charset.Charset.defaultCharset()
  private def send( key: String, commands: String* ) {
    val message = key + ":" + commands.mkString( "|" )
    try channel.send( charset.encode( message ), address )
    catch {
      case e: PortUnreachableException => // TODO implement graceful backoff for better performance --TG
    }
  }

  def counter( key: String, sampleRate: Option[ Float ] = None ): Counter = sampleRate match {
    case None =>
      new Counter {
        def add( delta: Int ) { send( key, delta.toString, "c" ) }
        def subtract( delta: Int ) { send( key, ( -delta ).toString, "c" ) }
      }
    case Some( rate ) =>
      new Counter {
        def add( delta: Int ) { send( key, delta.toString, "c", "@" + rate ) }
        def subtract( delta: Int ) { send( key, ( -delta ).toString, "c", "@" + rate ) }
      }
  }

  def time[ T ]( key: String, logOnFail: Boolean = false )( x: => T ): T = {
    val start = System.currentTimeMillis()
    def epilogue() { emitTime( key, System.currentTimeMillis() - start ) }

    val ret = try x
    catch { case e: Throwable if logOnFail => epilogue(); throw e }
    epilogue()

    ret
  }

  def emitTime( key: String, timeInMillis: Long ) { send( key, timeInMillis.toString, "ms" ) }

  def close() {
    try if ( _channel.isConnected ) _channel.disconnect()
    catch { case _: Exception => }

    try if ( _channel.isOpen ) _channel.close()
    catch { case _: Exception => }
  }
}