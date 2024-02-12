package org.planx.sh.solving

import org.planx.sh.problem
import org.planx.sh.problem.{Constant, Number, Term, Var}
import org.planx.sh.utility.DomainRequirements

/**
  * Numeric expressions are constructed using arithmetic operations from primitive numeric expressions,
  * which are values associated with tuples of domain objects by domain functions.
  */

trait FunExpression

/**
  * This class represents HDPL domain functions. A domain function associates values with tuples of domain objects.
  * Primitive numeric expressions are terms constructed from the function symbols of the domain applied to objects in the domain.
  * @param name - the name of the domain function (function symbol)
  * @param args - the arguments of the domain function (domain objects)
  * @param returnType - the return type for numeric functions is a number, while for object functions is an object (currently not supported)
  * @param argTypeExpr
  */
case class Function(name: String, args: List[Term], returnType: String = DomainRequirements.NOTYPE, argTypeExpr: List[Expression] = List()) extends FunExpression with Term {
  def evaluate(state: State, binding: Binding): problem.Number = state.evaluate(name, args, binding)
}

/** This class represents arithmetic and relation operations.
  * @param operator
  * @param arguments
  */
case class Operation(operator: String, arguments: List[FunExpression]) extends FunExpression {
  def calculate(state: State, binding: Binding): problem.Number = {
    if(!binding.isEmpty){
      val groundArguments = arguments.map(argument => argument match {
        case v: Var => Bindable(v).apply(binding).head
        case c: Constant => c
        case n: problem.Number => n
        case f: Function => f.evaluate(state, binding)
      })
      Calculator.calculate((operator, groundArguments))
    }
    else Number(0)
  }
}

case class Member(element: Any, list: List[Any]) extends FunExpression

/** This class represents the comparison operations (used in preconditions).
  * @param comparator
  * @param arguments
  */
case class Comparison(comparator: String, arguments: List[FunExpression]) {
  def evaluate(state: State, binding: Binding): Boolean = {
    if(binding.nonEmpty){
      val groundArguments = arguments.map {
        case v: Var => Bindable(v).apply(binding).head
        case c: Constant => c
        case n: Number => n
        case o: Operation => o.calculate(state, binding)
        case f: Function => f.evaluate(state, binding)
      }
      Calculator.compare((comparator, groundArguments))
    }
    else false
  }
}

/** This class represents actual calculations (on grounded terms) */
object Calculator {
  /** A method that performs relation operations
    * @param operation
    * @return
    */
  def compare(operation: (String, List[Any])): Boolean = {
    operation match {
      case ("<", arguments: List[Number])   => functionate(arguments, { case (x: Double, y: Double) => x < y })
      case ("<=", arguments: List[Number])  => functionate(arguments, { case (x: Double, y: Double) => x <= y })
      case (">", arguments: List[Number])   => functionate(arguments, { case (x: Double, y: Double) => x > y })
      case (">=", arguments: List[Number])  => functionate(arguments, { case (x: Double, y: Double) => x >= y })
      case ("=", arguments: List[Any])      => functionate(arguments, { case (x, y) => x == y })
      case ("!=", arguments: List[Any])     => functionate(arguments, { case (x, y) => x != y })
      case ("member", arguments: List[Term]) => {
        arguments.tail.head match {
          //case p: Params => p.parameters.contains(params.head)
          case _ => arguments.tail.contains(arguments.head)
        }

      }
      case _                              => throw new RuntimeException("Unsupported relation!")
    }
  }

  /** A method that performs arithmetic operations
    * @param operation
    * @return
    */
  def calculate(operation: (String, List[Term])): Number = {
    operation match {
      case ("+", arguments: List[Number])   => Number(arguments.foldLeft(0.0)((acc, v) => acc + v.n))
      case ("-", arguments: List[Number])   => Number(arguments.head.n - arguments.tail.foldLeft(0.0)((acc, v) => acc + v.n))
      case ("*", arguments: List[Number])   => Number(arguments.foldLeft(1.0)((acc, v) => acc * v.n))
      case ("/", arguments: List[Number])   => Number(arguments.head.n / arguments.tail.foldLeft(1.0)((acc, v) => acc * v.n))
      case ("^", arguments: List[Number])   => Number(compute(arguments.head.n, arguments.tail))
      case _                                => throw new RuntimeException("Unsupported arithmetic operation!")
    }
  }

  protected def functionate(list: List[Any], f: (Any, Any) => Boolean): Boolean = {
    list match {
      case Nil => true
      case e :: Nil => true
      case e1 :: rest => {
        (e1, rest.head) match {
          case (s: Number, r: Number) => f(s.n, r.n) && functionate(rest, f)
          case _ => f(e1, rest.head) && functionate(rest, f)
        }
      }
    }
  }

  protected def compute(base: Double, arguments: List[Number]): Double = if (arguments.isEmpty) base else compute(exponentiate(base, arguments.head.n), arguments.tail)
  protected def exponentiate(base: Double, exponent: Double): Double = if (exponent == 0) 1.0 else base * exponentiate(base, exponent - 1.0)

}