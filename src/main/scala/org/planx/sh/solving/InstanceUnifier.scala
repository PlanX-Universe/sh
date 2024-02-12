package org.planx.sh.solving

import org.planx.sh.problem.{Operator, Task, TaskList, Term}
import org.planx.sh.utility.Messaging

trait InstanceUnifier extends TaskUnifier {
  val name: String
  val blueprint: Bindable
  var arguments: List[Term]// = List()

  lazy val binding = blueprint.unify(arguments, Binding()) match {
      case Some(binding) => binding
      case None => throw new Exception(Messaging.printPlanningSystemComponentMessage("Task") + "No binding in: " + toString)
    }

  // Needs to be implemented for introspection.
  def toString: String

  /** Returns an iterator of possible decompositions. */
  def decompose2(state: State): Iterator[Binding]
  def decompose(state: State, domainTasks: List[Task], domainOperators: List[Operator]): Iterator[TaskList]

  /** Methods to "execute" or "undo" a task to/from the state.
    * This is generally only overridden in the OperatorTemplate subclass, because
    * that has an actual effect on the state. */
  def execute(state: State, binding: Binding): Unit = Unit
  def undo(state: State): Unit = Unit

  /** Method 'assignValues' assigns actual values to the task parameters
    * given a particular binding and mementos. It is used when the task is a part
    * of a decomposition and needs to be instantiated. */
  def assignValues(b: Binding) = this.arguments = Bindable(this.arguments).apply(b)
}

/** Unifies a Template, TaskList and InstanceUnifier */
trait TaskUnifier