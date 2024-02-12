package org.planx.sh.problem

import org.planx.sh.utility.Messaging
import org.planx.sh.solving.{Bindable, Binding, Calculator, Expression, ExpressionNil, FunExpression, Function, InstanceUnifier, State}

/** Case class Operator represents an actual instance of an OperatorTemplate */
trait OperatorInstance

abstract class Operator() {
  var _name: String = ""
  var parameters: List[Term] = List()
  var precondition: Expression = ExpressionNil()
  var delete: List[Effect] = List()
  var add: List[Effect] = List()
  var assignment: List[Assignment] = List()
  var cost: Double = 0.0

  def apply(_args: List[Term]) = new Instance {
    var arguments = _args
    val blueprint = Bindable(parameters)
    val name = _name// + "(" + arguments.mkString(",") + ")"
  }

  protected trait Instance extends OperatorInstance with InstanceUnifier {
    /** The complete binding consists of the bindings for the parameter variables,
      * and the bindings for the precondition variables different than those in the parameters
      */
    //var complete_binding = Binding()

    override def decompose2(state: State) = precondition.getBindings(state, binding)

    override def decompose(state: State, domainTasks: List[Task], domainOperators: List[Operator]) = Iterator.empty

    override def toString = name + "(" + arguments.mkString(",") + ")"

    protected def instantiateEffectParams(bindable: Bindable, binding: Binding, state: State): Array[Any] = {
      val accumulator: Option[List[Any]] = Some(Nil)
      /** for every argument 'arg' in the blueprint 'bindable' find an appropriate binding 'value' */
      val result = bindable.arguments.foldLeft(accumulator)((acc, argument) => acc match {
        case Some(listOfValues) => argument match {
          /** Try to get names from binding. */
          case v: Var => binding.get(v) match {
            case Some(value) => Some(listOfValues :+ value)
            case None => None
          }
          /** If the name is a non-symbol, it is interpreted as a pre-filled argument. */
          case f: Function => Some(listOfValues :+ state.evaluate(f.name, f.args, binding))
          case value => Some(listOfValues :+ value)
        }
        /** Skip when extraction of the argument list fails. */
        case None => None
      })
      /** Expect that a valid argument list can be found in an operator. */
      result match {
        case Some(values) => values.toArray
        case None => throw new Exception(Messaging.printPlanningSystemComponentMessage("Operator") + ": Could not extract arguments for the " + name + " operator!")
      }
    }

    /** Evaluated effects of the operator by calling 'instantiateEffectParams' method */
    def valuesToAdd(state: State) = {
      lazy val vta = add.flatMap {
        case a: Add => a.atom.arguments match {
          case args: List[Term] => List((a.atom.name, instantiateEffectParams(Bindable(args), binding, state)))
        }
        case f: ForallEffect => f.expr.getBindings(state, binding).flatMap(binding => {
          f.predicates.map({
            case a: Add => (a.atom.name, instantiateEffectParams(Bindable(a.atom.arguments), binding, state))
          })
        })
      }

      vta
    }

    def valuesToDelete(state: State) = {
      lazy val vtd = delete.flatMap {
        case d: Delete => d.atom.arguments match {
          case args: List[Term] => List((d.atom.name, instantiateEffectParams(Bindable(args), binding, state)))
        }
        case f: ForallEffect => f.expr.getBindings(state, binding).flatMap(binding => {
          f.predicates.map({
            case d: Delete => (d.atom.name, instantiateEffectParams(Bindable(d.atom.arguments), binding, state))
          })
        })
      }

      vtd
    }

    def valuesToUpdate(state: State) = {
      lazy val vtu = assignment.flatMap {
        case na: NumericAssignment => {
          na.evaluate(binding, state)
          List((na.function_name, na.function_args.toArray, na.function_value))
        }
      }
      vtu
    }

    /** 'execute' applies an operator to the state - it changes the state */
    override def execute(state: State, _binding: Binding): Unit = {
      for ((predicateName, arguments) <- valuesToDelete(state)) state.remove(predicateName, arguments)    //   valuesToDelete
      for ((predicateName, arguments) <- valuesToAdd(state)) state.add(predicateName, arguments) // valuesToAdd
      for ((functionName, arguments, value) <- valuesToUpdate(state)) state.update(functionName, arguments, value) //valuesToUpdate
    }

    /** Method 'undo' undoes an applied operator from the state - it returns the state as it was before applying this operator */
    override def undo(state: State): Unit = {
      for ((predicateName, arguments) <- valuesToAdd(state)) state.remove(predicateName, arguments) // valuesToAdd
      for ((predicateName, arguments) <- valuesToDelete(state)) state.add(predicateName, arguments) //valuesToDelete
      for ((functionName, arguments, value) <- valuesToUpdate(state)) state.update(functionName, arguments.dropRight(1) :+ value, arguments.last.asInstanceOf[Number]) // valuesToUpdate
    }
  }
}

trait Effect
trait Assignment extends Effect
case class Add(atom: Predicate) extends Effect
case class Delete(atom: Predicate) extends Effect
case class ForallEffect(expr: Expression, predicates: List[Effect], addList: Boolean = true) extends Effect

/**
  * Numeric effects use a selection of assignment operations in order to update the values of primitive numeric expressions.
  * The syntactic form of a numeric effect consists of an assignment operator (assign, increase, decrease, scale-up or scale-down),
  * one primitive numeric expression (lvalue), and a numeric expression (rvalue), which is an arithmetic expression whose terms and numbers are primitive numeric expressions.
  * For example, (increase p q) => p is lvalue, and (+ p q) is rvalue.
  * The assignment propositions would be as follows:
  * (assign p q)     => (= p q)
  * (increase p q)   => (= p (+ p q))
  * (decrease p q)   => (= p (- p q))
  * (scale-up p q)   => (= p (* p q))
  * (scale-down p q) => (= p (/ p q))
  * @param name - the name of the assignment operation
  * @param firstExpr - a primitive numeric expression, that is, a domain function
  * @param secondExpr - a number, a variable, or another primitive numeric expression
  */
case class NumericAssignment(name: String, firstExpr: FunExpression, secondExpr: FunExpression) extends Assignment {
  var function_name = ""
  var function_args: List[Term] = List()
  var function_value: Number = Number(0)

  def evaluate(binding: Binding, state: State) = {
    firstExpr match {
      case f: Function =>
        function_name = f.name
        name match {
          case "assign" => function_value = helper(f, secondExpr, binding, "", state)
          case "increase" => function_value = helper(f, secondExpr, binding, "+", state)
          case "decrease" => function_value = helper(f, secondExpr, binding, "-", state)
          case "scale-up" => function_value = helper(f, secondExpr, binding, "*", state)
          case "scale-down" => function_value = helper(f, secondExpr, binding, "/", state)
        }
      case _ => throw new RuntimeException("The lvalue of a numeric assignment should be a primitive numeric expression, that is, a domain function.")
    }
  }

  protected def helper(f: Function, expr: FunExpression, binding: Binding, operator: String, state: State) = {
    function_args = Bindable(f.args).apply(binding)
    val lv = f.evaluate(state, binding)
    function_args = function_args :+ lv
    expr match {
      case n: Number => if(operator.isEmpty) n else Calculator.calculate(operator, List(lv, n))
      case v: Var => val value = Bindable(v).apply(binding).head.asInstanceOf[Number]
        if(operator.isEmpty) value else Calculator.calculate(operator, List(lv, value))
      case ff: Function => val rv = ff.evaluate(state, binding)
        if(operator.isEmpty) rv else Calculator.calculate(operator, List(lv, rv))
    }
  }
}
case class ObjectAssignment(arg1: FunExpression, arg2: Term) extends Effect with Assignment
case class EmptyEffect() extends Effect