package org.planx.sh.problem

import org.planx.sh.utility.DomainRequirements
import org.planx.sh.solving.{Expression, ExpressionNil, FunExpression, TaskUnifier}

import scala.collection.immutable._
import scala.collection.mutable.Set

trait Term

trait Value extends Term

/** HPDL variable */
case class Var(name: Symbol, _type: String = DomainRequirements.NOTYPE) extends Term with FunExpression {
  var value: Term = null
  override def toString = if (value != null) value.toString else "?" + name.toString.substring(1)
}

/** HPDL constant; it is a value */
case class Constant(c: String) extends Value with FunExpression {
  override def toString = c
}

/** HPDL number; it is a value */
case class Number(n: Double) extends Value with FunExpression {
  override def toString = n.toString
}

case class Predicate(name: String, arguments: List[Term] = List(), argsTypes: Set[(Int, String)] = Set(), argsExpr: Expression = ExpressionNil()) extends TaskUnifier {
  def cloneMe() = Predicate(name, arguments, argsTypes, argsExpr)
}