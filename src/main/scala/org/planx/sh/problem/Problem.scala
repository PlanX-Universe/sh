package org.planx.sh.problem

import org.planx.sh.solving.State

case class Problem(name: String, domainName: String, requirements: List[String], objects: Objects, state: State, goalTaskList: TaskList) {}

case class ProblemObject(_type: String, _value: String)

case class Objects(objects: List[ProblemObject] = List()) {
  def print(): Unit = {
    objects foreach(o => println(o))
  }
}