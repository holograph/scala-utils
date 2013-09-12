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

 * Created by tomer on 9/12/13.
 */

package com.tomergabel.util.servlet

import javax.servlet.ServletContext

/**
 * A hackish set of utility methods to extract the set of available runtime ports from a servlet container's
 * ServletContext instance. Unfortunately the Serlvet API doesn't expose host-names or port numbers, likely
 * intentionally, but these are often required for e.g. registering your container with a runtime discovery
 * service.
 *
 * So far this has been tested successfully with:
 * - Jetty 6.1.26 (embedded)
 * - Jetty 8.1.3 (embedded)
 *
 */
object LateBoundPortResolver {
  private val Jetty = "jetty/(.+)".r

  def resolveExternalJettyPorts( context: ServletContext ): Seq[ Int ] = {
    import java.lang.reflect.{Array => JArray }

    val contextHandler = context.getClass.getMethod( "getContextHandler" ).invoke( context )
    val server = contextHandler.getClass.getMethod( "getServer" ).invoke( contextHandler )
    val connectors = server.getClass.getMethod( "getConnectors" ).invoke( server )
    val ports =
      for ( idx <- 0 until JArray.getLength( connectors );
            connector = JArray.get( connectors, idx ) )
        yield connector.getClass.getMethod( "getPort" ).invoke( connector ).asInstanceOf[ Int ]
    ports.filter { _ > 0 }
  }

  def resolveExternalServletContainerPorts( context: ServletContext ): Seq[ Int ] =
    context.getServerInfo match {
      case Jetty(_) => resolveExternalJettyPorts( context )
      case info => throw new Exception( s"Failed to identify servlet container $info" )
    }
}
