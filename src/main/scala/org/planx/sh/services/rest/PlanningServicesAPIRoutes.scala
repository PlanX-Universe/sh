package org.planx.sh.services.rest

import akka.actor.ActorSystem
import org.planx.sh.services.PlanningServices
import akka.http.scaladsl.server.{HttpApp, Route}

class PlanningServicesAPIRoutes extends HttpApp {
  implicit def actorRefFactory = ActorSystem("SH Planning System")

  val services: Route = {
    pathPrefix("services") {
      path("status") {
        get {
          ctx =>
            ctx.complete("The planning system is up and running...")
        }
      } ~
        pathPrefix("storing") {
          path("domain-and-problem") {
            post {
              entity(as[String]) { in => {
                try {
                  val input_list = in.split("EOD")
                  val domain = input_list(0)
                  val problem = input_list(1)
                  val result = PlanningServices.storeProvidedDomainAndProblemInString(domain, problem)
                  println("Printing results")
                  println(result)
                  complete(result)
                } catch {
                  case e: Exception => complete(e.getMessage)
                }
              }
              }
            }
          }
        }~
        pathPrefix("plan-generation") {
          path("problem-in-string") {
            post {
              entity(as[String]) { in => {
                try {
                  val result = PlanningServices.planWithProvidedProblemInString(in, 1)
                  println("Printing results")
                  println(result)
                  complete(result)
                } catch {
                  case e: Exception => complete(e.getMessage)
                }
              }
              }
            }
          } ~
            path("domain-and-problem") {
              post {
                entity(as[String]) { in => {
                  try {
                    val input_list = in.split("EOD")
                    val domain = input_list(0)
                    val problem = input_list(1)
                    val result = PlanningServices.planWithProvidedDomainAndProblemInString(domain,problem, 1)
                    println("Printing results")
                    println(result)
                    complete(result)
                  } catch {
                    case e: Exception => complete(e.getMessage)
                  }
                }
                }
              }
            } ~
            path(Segment / Segment) { (domain, problem) =>
              get {
                entity(as[String]) { in => {
                  try {
                    val result = PlanningServices.planWithGivenDomainAndProblemNameReturnString(domain, problem, 1)
                    complete(result)
                  } catch {
                    case e: Exception => complete(e.getMessage)
                  }
                }
                }
              }
            }
        } ~
        pathPrefix("syntax-verification") {
          path("problem") {
            post {
              entity(as[String]) { in => {
                try {
                  val result = PlanningServices.checkProvidedProblemInString(in)
                  println("Printing results")
                  println(result)
                  complete(result)
                } catch {
                  case e: Exception => complete(e.getMessage)
                }
              }
              }
            }
          } ~
            path("domain") {
              post {
                entity(as[String]) { in => {
                  try {
                    val result = PlanningServices.checkProvidedDomainInString(in)
                    println("Printing results")
                    println(result)
                    complete(result)
                  } catch {
                    case e: Exception => complete(e.getMessage)
                  }
                }
                }
              }
            }
        }
    }
  }

  override def routes: Route = services
}

