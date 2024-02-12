package org.planx.sh.problem

import org.planx.sh.problem
import org.planx.sh.solving.{Expression, ExpressionAnd, ExpressionAtomic, ExpressionComparison, ExpressionForall, ExpressionImply, ExpressionNil, ExpressionNot, ExpressionOr, Function, InstanceUnifier, TaskUnifier}

case class DomainMethod(name: String = "", precondition: Expression, tasks: TaskList)
case class DomainTask(name: String, parameters: List[Term])
case class DomainOperator(name: String, parameters: List[Term], precondition: Expression, add: List[Effect], delete: List[Effect], assignment: List[Assignment], cost: Double)
case class DomainType(name: String, supertype: String)

case class Domain(name: String,
                  requirements: List[String],
                  types: List[DomainType],
                  predicates: List[Predicate],
                  functions: List[Function],
                  _operators: List[DomainOperator],
                  uncoupledTasks: List[(DomainTask, List[DomainMethod])],
                  axioms: List[Axiom]) {
  private var coupled: List[Task] = List()

  val operators: List[Operator] = _operators map (o => new Operator(){
    _name = o.name
    parameters = o.parameters
    precondition = o.precondition
    add = o.add
    delete = o.delete
    assignment = o.assignment
    cost = o.cost
  })
  val tasks: List[Task] = {
    coupled = coupleUncoupledTasks(uncoupledTasks)
    coupled
  }

  /** Creates complete methods for each task.
    * Once the domain is completely processed, that is, all operators and tasks are known,
    * methods can contain compete templates of operators and tasks.
    */
  private def coupleUncoupledTasks(uncoupledTasks: List[(DomainTask, List[DomainMethod])]): List[Task] = {
    uncoupledTasks map (ut => {
      val task = new Task(){
        _name = ut._1.name
        parameters = ut._1.parameters
      }
      ut._2 foreach (method => {
        task.addMethod(Method(method.name, checkPreconditionsForAxioms(method.precondition), method.tasks))
      })
      task
    }
      )
  }

  private def checkPreconditionsForAxioms(precondition: Expression): Expression = {
    precondition match {
      case atomic: ExpressionAtomic => iterateAxioms(atomic)
      case and: ExpressionAnd => ExpressionAnd(checkPreconditionsForAxioms(and.left), checkPreconditionsForAxioms(and.right))
      case not: ExpressionNot => ExpressionNot(checkPreconditionsForAxioms(not.precond))
      case or: ExpressionOr => ExpressionOr(checkPreconditionsForAxioms(or.left), checkPreconditionsForAxioms(or.right))
      case imply: ExpressionImply => ExpressionImply(checkPreconditionsForAxioms(imply.first), checkPreconditionsForAxioms(imply.second))
      case comparison: ExpressionComparison => comparison
      case forall: ExpressionForall => ExpressionForall(forall.params, checkPreconditionsForAxioms(forall.firstExpr), checkPreconditionsForAxioms(forall.secondExpr))
      case nil: ExpressionNil => nil
    }
  }

  private def iterateAxioms(atomic: ExpressionAtomic): Expression = {
    var resultExpression: Expression = atomic
    for (axiom <- axioms )
      if (axiom.n.equals(atomic.predicateName)) resultExpression = axiom
    resultExpression
  }

  private def findTask(p: Predicate) =  tasks.filter(task => task._name.equals(p.name) & task.parameters.size == p.arguments.size)

  private def findOperator(p: Predicate) = operators.filter(o => o._name.equals(p.name) & o.parameters.size == p.arguments.size)

  def preprocessGoalTaskList(goalTaskList: TaskList): List[List[InstanceUnifier]] = {
    val goalPredicates = translateToPredicates(goalTaskList)
    goalPredicates map (gp => gp map (p => findTask(p) match {
      case List() => findOperator(p) match {
        case List() => throw new RuntimeException("The goal task " + p + " cannot be found in the domain specification.")
        case o: Operator => o(p.arguments)
      }
      case l => /*fillTask(l.head)*/l.head(p.arguments)
    }))
  }

  private def translateToPredicates(tl: TaskList): List[List[Predicate]] = {
    tl.ordering match {
      case "sequence" => handlePSequence(tl.tasks)
      case "unordered" => handlePUnordered(tl.tasks)
    }
  }

  private def handlePSequence(tasks: List[TaskUnifier]): List[List[Predicate]] = {
    /** Check if all members of tasks are of type InstanceUnifier **/
    val allInstances = tasks filter (t => t.isInstanceOf[TaskList])
    var list: List[List[Predicate]] = List()
    if(allInstances.isEmpty) List(tasks map (t => t.asInstanceOf[Predicate]))
    else {
      for (t <- tasks) t match {
        case ti: InstanceUnifier => list = predicateHandler(problem.Predicate(ti.name, ti.arguments), list) // TODO: check the Predicate, not tested
        case ntl: TaskList => list = pTaskListHandler(ntl, list)
      }
      list
    }
  }

  private def handlePUnordered(tasks: List[TaskUnifier]): List[List[Predicate]] = {
    /** Check if all members of tasks are of type InstanceUnifier **/
    val allInstances = tasks filter (t => t.isInstanceOf[TaskList])
    var finalList: List[List[Predicate]] = List()
    var list: List[List[Predicate]] = List()
    if(allInstances.isEmpty) perm(tasks.size, tasks map (t => t.asInstanceOf[Predicate]))
    else {
      val permutations = perm(tasks.size, tasks)
      for (pTasks <- permutations){
        finalList = finalList ::: list
        list = List()
        for (pTask <- pTasks) pTask match {
          case ti: InstanceUnifier => list = predicateHandler(problem.Predicate(ti.name, ti.arguments), list) // TODO: check the Predicate, not tested
          case ntl: TaskList => list = pTaskListHandler(ntl, list)
        }
      }
      finalList = finalList ::: list
      finalList
    }
  }

  private def predicateHandler(p: Predicate, list: List[List[Predicate]]): List[List[Predicate]] = {
    if(list.isEmpty) list :+ List(p)
    else for {ll <- list} yield ll :+ p
  }

  private def pTaskListHandler(tl: TaskList, list: List[List[Predicate]]): List[List[Predicate]] = {
    val tmpTasks = translateToPredicates(tl)
    if(list.isEmpty) tmpTasks
    else for {
      tmpTask <- tmpTasks
      ll <- list
    } yield ll ::: tmpTask
  }

  private def perm[T](n: Int, l: List[T]): List[List[T]] =  n match {
    case 0 => List(List())
    case _ => for(el <- l; sl <- perm(n-1, l filter (_ != el))) yield el :: sl
  }

  override def toString = "Domain:\n" +
    "name: " + name + "\n" +
    "requirements: " + requirements + "\n" +
    "types: " + types + "\n" +
    "predicates: " + predicates + "\n" +
    "functions: " + functions + "\n" +
    "operators" + operators + "\n" +
    "tasks: " + tasks + "\n"

}