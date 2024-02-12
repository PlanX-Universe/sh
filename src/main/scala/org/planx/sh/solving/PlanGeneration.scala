package org.planx.sh.solving

import grizzled.slf4j.Logging
import org.planx.sh.problem.{Operator, OperatorInstance, Task, TaskInstance, TaskList}
import org.planx.sh.utility.{Messaging, Statistics}
import org.planx.sh.solution.Plan

import scala.collection.mutable._
import compat.Platform._
import management._

case class PlanGeneration(_state: State, domainTasks: List[Task], domainOperators: List[Operator]) extends Logging {
  private var nPlans = 0
  var plans = ListBuffer[Plan]()

  info(Messaging.printPlanningSystemMessage + "Starting the plan-generation process.")

  def process(goalTaskList: List[List[InstanceUnifier]], numPlans: Int): Option[ListBuffer[Plan]] = {
    nPlans = numPlans
    val state = this._state.clone

    val tStart = currentTime

    try {
      search(goalTaskList, List(), state, new Stack[InstanceUnifier])

      Statistics.planningTime = (currentTime - tStart)
      Statistics.memory = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed
      Statistics.numberOfPlans = plans.size

      if(plans.isEmpty) None
      else {
        Statistics.planLength = plans.reverse.head.length
        info(Messaging.printPlanningSystemMessage + "Finished the plan-generation process.")
        info(Messaging.printPlanningSystemMessage + "Found plan(s).")
        Some(plans)
      }
    } catch {
      case e: StackOverflowError => error(Messaging.printPlanningSystemMessage + " Could not find a plan due to stack overflow.")
        Statistics.planningTime = (currentTime - tStart)
        Statistics.memory = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed
        Statistics.numberOfPlans = plans.size
      None
    }
  }

  private def search(l: List[List[InstanceUnifier]], r: List[List[InstanceUnifier]], state: State, s: Stack[InstanceUnifier]): Unit = {
    def helper(left: List[List[InstanceUnifier]], right: List[List[InstanceUnifier]]): Boolean = {
      Statistics.numberOfRecursiveCalls += 1
      (left.head.isEmpty, right.isEmpty) match {
        case (true, false) => return helper(List(right.head), right.tail)
        case (true, true) => 
          plans += Plan(s)
          return !(plans.size < nPlans)
        case (_, _) =>
          left.head.head match {
            case (t: TaskInstance) =>
              Statistics.numberOfEvaluatedStates += 1
              for (tl <- t.decompose(state, domainTasks, domainOperators)) {
                Statistics.numberOfDecomposedTasks += 1
                val instanceList = translateToInstances(tl)
                if(instanceList.nonEmpty) {
                  for (instance <- instanceList) {
                    if(helper(List(List(instance.head)), ((instance.tail :::  left.head.tail) :: left.tail ::: right) filter (_ != Nil))) return true
                  }
                } else {
                  if (helper(List(left.head.tail) ++ left.tail, right)) return true
                }
              }
              false
            case (o: OperatorInstance) =>
              val temp = o.decompose2(state)
              Statistics.numberOfEvaluatedStates += 1
              if(temp.hasNext){
                val nextBinding = temp.next()
                o.execute(state, nextBinding)
                if(!o.name.startsWith("bk-")) s.push(o)
                (left.head.tail.isEmpty, right.isEmpty) match {
                  case (false, false) =>
                    if(helper(List(left.head.tail) ++ left.tail, right)) return true
                    for (r <- right) {
                      if(helper(List(r), List(left.head.tail) ++ left.tail ++ right diff List(r))) return true
                    }
                  case (true, false) =>
                    for (r <- right) {
                      if(helper(List(r), right diff List(r))) return true
                    }
                  case (_, _) => if(helper(List(left.head.tail) ++ left.tail, List())) return true
                }
                o.undo(state)
                if(!o.name.startsWith("bk-")) s.pop
                return false
              } else return false
          }
      }
    }
    helper(l, r)
  }

  def flatten(taskList: TaskList): List[List[InstanceUnifier]] = {
    taskList.ordering match {
      case "sequence" => {
        var temp = List[List[InstanceUnifier]]()
        for (t <- taskList.tasks) {
          t match {
            case unifier: InstanceUnifier =>
              if (temp.isEmpty) temp = List(List(unifier))
              else temp = temp map (e => e :+ unifier)
            case _ =>
              val r = flatten(t.asInstanceOf[TaskList])
              if (temp.isEmpty) temp = r
              else temp = r.flatMap(e => temp map (t => t ::: e))
          }
        }
        temp
      }
    }
  }

  private def translateToInstances(tl: TaskList): List[List[InstanceUnifier]] = {
    tl.ordering match {
      case "sequence" =>  handleSequence(tl.tasks) match {
        case Nil => List()
        case a => List(a)
      }
      // TODO: case "unordered" => handleUnordered(tl.tasks)
    }
  }

  private def handleSequence(tasks: List[TaskUnifier]): List[InstanceUnifier] = tasks match {
    case Nil => List()
    case head :: Nil => List(head.asInstanceOf[InstanceUnifier])
    case head :: tail => head match {
      case ti: InstanceUnifier => ti :: handleSequence(tail)
    }
  }
}