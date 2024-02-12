package org.planx.sh.parsing.hpdl

import grizzled.slf4j.Logging
import org.planx.sh
import org.planx.sh.problem.{Constant, Number, Objects, Predicate, Problem, ProblemObject, Term, Var}
import org.planx.sh.solving.State
import org.planx.sh.utility.{DomainRequirements, Messaging}

import scala.collection.mutable.ListBuffer
import scala.io.Source._

class HPDLProblemParser extends HPDLParser with Logging {

  domain_parser_on = false
  dvars = new ListBuffer[Var]()

  lazy val problem = "(" ~> "define" ~> problem_name ~ domain_name ~ require_def ~ objects_dec ~ init ~ goal_tasks <~ ")" ^^ {
    case pn ~ dn ~ rd ~ od ~ i ~ gt =>
      for (o <- od.objects) i.add(o._type, Array(Constant(o._value)))
      sh.problem.Problem(pn, dn, rd, od, i, gt)
  }

  lazy val problem_name = "(" ~> "problem" ~> name <~ ")"
  lazy val domain_name = "(" ~> ":domain" ~> name <~ ")"

  lazy val objects_dec = opt(objects_def_helper) ^^ {
    case Some(odh) => Objects(odh.flatten)
    case None => Objects()
  }

  lazy val objects_def_helper = "(" ~> ":objects" ~> rep(objects_list) <~ ")"
  lazy val objects_list = rep1(name) ~ opt(supertype) ^^ {
    case objectNames ~ Some (st) => objectNames map (on => ProblemObject(st, on))
    case objectNames ~ None => objectNames map (on => ProblemObject(DomainRequirements.OBJECT, on))
  }

  lazy val init: Parser[State] = "(" ~> ":init" ~> rep(init_elements) <~ ")" ^^ {
    case facts =>  {
      val s = new State
      facts foreach (fact => s.add(fact.name, fact.arguments.toArray))
      s
    }
  }

  lazy val init_elements: Parser[Predicate] = simple_predicate| proper_predicate | init_fluent
  lazy val init_fluent = "(" ~> "=" ~> basic_function_term ~ fluent_value <~ ")" ^^ {
    case bft ~ fv => if(bft._2.isEmpty) Predicate(bft._1, List(fv))
    else Predicate(bft._1, bft._2 ::: List(fv))
  }
  lazy val basic_function_term: Parser[(String, List[Term])] = function_symbol ^^ {case fs => (fs, List())} | "(" ~> function_symbol ~ rep(name) <~ ")" ^^ {case fs ~ ns => (fs, ns map (n => Constant(n)))}
  lazy val fluent_value: Parser[Term] = number ^^ {n => Number(n.toDouble)} | name ^^ {n => Constant(n)}

  lazy val goal_tasks = "(" ~> ":goal-tasks" ~> task_list <~ ")"
}

object HPDLProblemParser extends HPDLProblemParser {

  private def parseStringToObject(input: String) = {
    info(Messaging.printPlanningSystemMessage + "Processing the problem specification.")
    val phrase_problem = phrase(problem)
    val scan_phrased_problem = phrase_problem (new lexical.Scanner(input))
    scan_phrased_problem match {
      case Success(result, _) => {
        info(Messaging.printPlanningSystemMessage + "The problem specification is correctly processed.")
        Some(result).get
      }
      case Failure(msg, next) => error(Messaging.printPlanningSystemMessage + "Failure at [" + next.pos.line +"."+ next.pos.column + "] - " + msg)
      case Error(msg, next) => error(Messaging.printPlanningSystemMessage + "Error at ["+ next.pos.line +"."+ next.pos.column + "] - " + msg)
    }
  }

  private def parseFileToObject(file: String) = parseStringToObject(fromFile(file).mkString)

  def processProblemFileToObject(file: String) = parseFileToObject(file) match {
    case p: Problem => p
    case _ => throw new RuntimeException(Messaging.printPlanningSystemMessage + "Processing failed. Please fix the problem file according to the HPDL syntax")
  }

  def processProblemStringToObject(problemString: String) = parseStringToObject(problemString) match {
    case p: Problem => p
    case _ => throw new RuntimeException(Messaging.printPlanningSystemMessage + "Processing failed. Please fix the problem file according to the HPDL syntax")
  }

  def checkProblemStringSyntax(input: String) = {
    info(Messaging.printPlanningSystemMessage + "Processing the problem specification.")
    phrase(problem)(new lexical.Scanner(input)) match {
      case Success(result, _) => info(Messaging.printPlanningSystemMessage + "The problem specification is correctly formulated.")
      case Failure(msg, next) => error(Messaging.printPlanningSystemMessage + "Failure at [" + next.pos.line +"."+ next.pos.column + "] - " + msg)
      case Error(msg, next) => error(Messaging.printPlanningSystemMessage + "Error at ["+ next.pos.line +"."+ next.pos.column + "] - " + msg)
    }
  }

  def checkProblemFileSyntax(file: String) = checkProblemStringSyntax(fromFile(file).mkString)
}