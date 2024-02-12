package org.planx.sh.services

import org.planx.sh.parsing.hpdl.{HPDLDomainParser, HPDLProblemParser}
import org.planx.sh.problem.Domain
import org.planx.sh.solution.Plan
import org.planx.sh.utility.{Resources, Statistics}
import org.planx.sh.solving.PlanGeneration
import org.planx.sh.storing.ResourceStorer

import compat.Platform._
import scala.collection.mutable.ListBuffer

object PlanningServices {
  def planWithProvidedProblemInString(problemToBeSolved: String, numberOfPlans: Int = 1): String = {
    val problem = HPDLProblemParser.processProblemStringToObject(problemToBeSolved)
    val domain = HPDLDomainParser.processDomainFileToObject(Resources.getDomainPath(problem.domainName))
    val state = problem.state
    val goal = domain.preprocessGoalTaskList(problem.goalTaskList)

    val plans = PlanGeneration(state, domain.tasks, domain.operators).process(goal, numberOfPlans)
    var result = ""
    for ((p, i) <- plans.zipWithIndex) {
      result = p.toString
    }
    result
  }

  def storeProvidedDomainAndProblemInString(domainToBeSolved: String, problemToBeSolved: String): String = {
    val problem = HPDLProblemParser.processProblemStringToObject(problemToBeSolved)
    val domain = HPDLDomainParser.processDomainStringToObject(domainToBeSolved)
    var result = ""
    if (ResourceStorer.storeDomainFileInRepository(domain.name, domainToBeSolved)) {
      if (ResourceStorer.storeProblemFileInRepository(domain.name, problem.name, problemToBeSolved)) {
        result = "Domain and problem stored successfully."
      } else {
        result = "Failed to store the problem. Domain stored successfully. Make sure problem name and domain name are different."
      }
    } else {
      result = "Failed to store the domain."
    }
    result
  }

  def planWithProvidedDomainAndProblemInString(domainToBeSolved: String, problemToBeSolved: String, numberOfPlans: Int = 1): String = {
    val problem = HPDLProblemParser.processProblemStringToObject(problemToBeSolved)
    val domain = HPDLDomainParser.processDomainStringToObject(domainToBeSolved)
    val state = problem.state
    val goal = domain.preprocessGoalTaskList(problem.goalTaskList)

    val plans = PlanGeneration(state, domain.tasks, domain.operators).process(goal, numberOfPlans)
    var result = ""
    for ((p, i) <- plans.zipWithIndex) {
      result = p.toString
    }
    result
  }

  def checkProvidedProblemInString(problemToBeChecked: String): String = {
    try {
      HPDLProblemParser.processProblemStringToObject(problemToBeChecked)
      "The problem specification is correctly formulated."
    } catch {
      case e: RuntimeException => "Processing failed. Please fix the problem string according to the HPDL syntax"
    }
  }

  def checkProvidedDomainInString(domainToBeChecked: String): String = {
    try {
      HPDLDomainParser.processDomainStringToObject(domainToBeChecked)
      "The domain specification is correctly formulated."
    } catch {
      case e: RuntimeException => "Processing failed. Please fix the domain string according to the HPDL syntax"
    }
  }

  def planWithGivenDomainAndProblemNameReturnString(domainName: String, problemName: String, numberOfPlans: Int = 1): String = {
    val domain = HPDLDomainParser.processDomainFileToObject(Resources.getDomainPath(domainName))
    val problem = HPDLProblemParser.processProblemFileToObject(Resources.getProblemPath(domainName, problemName))
    val state = problem.state
    val goal = domain.preprocessGoalTaskList(problem.goalTaskList)

    val plans = PlanGeneration(state, domain.tasks, domain.operators).process(goal, numberOfPlans)
    var result = ""
    for ((p, i) <- plans.zipWithIndex) {
      result = p.toString
    }
    result
  }

  def planWithGivenDomainObjectAndProblemPathReturnPlan(domain: Domain, domainName: String, problemPath: String, numberOfPlans: Int = 1): Option[ListBuffer[Plan]] = {
    val problem = HPDLProblemParser.processProblemFileToObject(problemPath)
    val state = problem.state
    val goal = domain.preprocessGoalTaskList(problem.goalTaskList)

    PlanGeneration(state, domain.tasks, domain.operators).process(goal, numberOfPlans)
  }

  def planWithGivenDomainAndProblemNamePrintPlans(domainName: String, problemName: String, numberOfPlans: Int = 1): Unit = {
    val domainPath = Resources.getDomainPath(domainName)

    val start_time = currentTime
    val domain = HPDLDomainParser.processDomainFileToObject(domainPath)
    val problem = HPDLProblemParser.processProblemFileToObject(Resources.getProblemPath(domainName, problemName))
    Statistics.parsingTime = (currentTime - start_time)

    val state = problem.state
    val goal = domain.preprocessGoalTaskList(problem.goalTaskList)

    PlanGeneration(state, domain.tasks, domain.operators).process(goal, numberOfPlans) match {
      case Some(plans) =>
        for ((p, i) <- plans.reverse.zipWithIndex) {
          println("Plan %d:".format(i + 1))
          p.print
          println
        }
      case None => "Planning process failed to deliver result."
    }
    Statistics.print
  }
}