package org.planx.sh.problem

import org.planx.sh.solving._

trait TaskInstance

abstract class Task() {
  var _name = ""
  var parameters: List[Term] = List()
  var methods: List[Method] = List()

  def addMethod(m: Method) = methods = methods :+ m

  def apply(_args: List[Term]) = new Instance {
    val blueprint = Bindable(parameters)
    var arguments = _args
    val name = _name
  }

  override def toString = _name + "(" + parameters.mkString(",") + methods +")"

  protected trait Instance extends TaskInstance with InstanceUnifier {
    /** Return an iterator over possible bindings for the first precondition that has at least a single valid binding. */
    override def decompose(state: State, domainTasks: List[Task], domainOperators: List[Operator]) = {
      //println("state: " + state)
      val i = (for (
        m <- methods.iterator
      ) yield {
        m.precondition.getBindings(state, binding).map(instantiateTaskList(_, deepCloneTaskList(m.taskList, domainTasks, domainOperators)))

      }).find(_.nonEmpty).getOrElse(Iterator.empty)
      i
    }

    override def decompose2(state: State) = Iterator.empty

    override def toString = name + "(" + arguments.mkString(",") + ")"

    private def instantiateTaskList(b: Binding, tl: TaskList): TaskList = {
      TaskList(tl.ordering, tl.tasks map ({
        case ti: InstanceUnifier => instantiateTask(b, ti)
        case ntl: TaskList => instantiateTaskList(b, ntl)
      }))
    }

    private def instantiateTask(b: Binding, t: InstanceUnifier): InstanceUnifier = {
      t.assignValues(b)
      t
    }

    private def instantiateTasks(b: Binding, tasks: List[InstanceUnifier]): List[InstanceUnifier] = {
      for (task <- tasks) task.assignValues(b)
      tasks
    }

    private def deepCloneTaskList(tl: TaskList, domainTasks: List[Task], domainOperators: List[Operator]): TaskList = {
      TaskList(tl.ordering, tl.tasks map ({
        case p: Predicate => fillTask(p, domainTasks, domainOperators)
        case ntl: TaskList => deepCloneTaskList(ntl, domainTasks, domainOperators)
      }))
    }

    private def fillTask(p: Predicate, domainTasks: List[Task], domainOperators: List[Operator]): InstanceUnifier = {
      val ff = (domainTasks filter (c => c._name.equals(p.name)))
      if (ff.isEmpty) {
        try {
          val f = (domainOperators filter (c => c._name.equals(p.name))).head
          f(p.arguments)
        } catch {
          case e: NoSuchElementException => throw new RuntimeException("No operator for " + p.name)
        }
      } else {
        val fa = ff.head
        fa(p.arguments)
      }
    }
  }
}

case class Method(name: String = "", precondition: Expression, taskList: TaskList)
case class TaskList(ordering: String, tasks: List[TaskUnifier]) extends TaskUnifier {
  def isEmpty = ordering.equals("") & tasks.isEmpty
}