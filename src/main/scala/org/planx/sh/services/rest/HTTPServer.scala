package org.planx.sh.services.rest

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import akka.http.scaladsl._

import scala.concurrent.ExecutionContextExecutor

object HTTPServer extends App {
  /** an ActorSystem to host the application */
  implicit val system = ActorSystem("shplanner-api")
  val shRestServices = new PlanningServicesAPIRoutes
  val apiRoutes = shRestServices.routes

  val config = ConfigFactory.load()
  val host = config.getString("http.server.host") // Gets the host and a port from the configuration
  val port = config.getInt("http.server.port")

  implicit val ec: ExecutionContextExecutor = system.dispatcher // bindingFuture.map requires an implicit ExecutionContext


  implicit val materializer: ActorMaterializer = ActorMaterializer()

  Http().bindAndHandle(apiRoutes, host, port)
}
