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

   Created by tomer on 9/12/13.
*/

package com.tomergabel.util.servlet

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.mortbay.jetty.Server
import org.mortbay.jetty.nio.SelectChannelConnector
import org.mortbay.jetty.servlet.Context
import javax.servlet.{ServletContextEvent, ServletContextListener}
import org.mortbay.jetty.bio.SocketConnector

class LateBoundPortResolverWithJetty6Test extends WordSpec with ShouldMatchers {

  "LateBoundPortResolver.resolveExternalJettyPorts" should {

    def testServerForPorts( server: Server, ports: Int* ) {
      val context = new Context( server, "/" )
      val listener = new ServletContextListener {
        var ports: Option[ Seq[ Int ] ] = None
        def contextDestroyed( sce: ServletContextEvent ) {}
        def contextInitialized( sce: ServletContextEvent ) {
          ports = Some( LateBoundPortResolver.resolveExternalJettyPorts( sce.getServletContext ) )
        }
      }
      context.addEventListener( listener )
      server.start()
      server.stop()

      listener.ports should be === Some( ports.toSeq )
    }

    "correctly resolve a SocketConnector port" in {
      val server = new Server()
      val connector = new SocketConnector()
      connector.setPort( 1234 )
      server.addConnector( connector )
      testServerForPorts( server, 1234 )
    }

    "correctly resolve a SelectChannelConnector port" in {
      val server = new Server()
      val connector = new SelectChannelConnector()
      connector.setPort( 1234 )
      server.addConnector( connector )
      testServerForPorts( server, 1234 )
    }
  }
}
