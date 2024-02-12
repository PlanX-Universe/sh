package org.planx.sh.solving

import org.planx.sh.problem.{Term, Var}

import scala.collection.immutable._
import scala.language.{implicitConversions, postfixOps}

trait Expression {
  def getBindings(state: State, binding: Binding): Iterator[Binding]
  def not(inner: Expression) = ExpressionNot(inner)
  def &(that: Expression) = ExpressionAnd(this, that) // Conjunction of two preconditions with AND
  def ||(that: Expression) = ExpressionOr(this, that) // Disjunction of two preconditions with OR
  def imply(that: Expression) = ExpressionOr(ExpressionNot(this), that)

  //override def toString: String
}

trait ExpressionConversions  {
  def fProduct(product: Product): Iterator[Term] = {
    product.productIterator.flatMap {
      case t: Term => Iterator(t)
      case p: Product => fProduct(p)
    }
  }

  private def bindable(params: Any): Bindable = params match {
    case params: List[Term] => Bindable(params)
    case params: Term => Bindable(List(params))
    case params: Product => Bindable(fProduct(params).toList)
    case _ => throw new RuntimeException("Somethings wrong with " + params)
  }

  /** Task to create a 'forall' precondition. */
  def forall(params: List[Var], leftExpr: Expression, rightExpr: Expression) = ExpressionForall(params, leftExpr, rightExpr)

  implicit def createAtomic(predicateName: String) = new {
    def ->(params: Any) = ExpressionAtomic(predicateName, bindable(params))
  }

  implicit def createComparison(comparator: String) = new {
    def compare(params: List[FunExpression]) = ExpressionComparison(Comparison(comparator, params))
  }
}

/**  This class represents an iterator over all the possible bindings that can
  *  satisfy an empty logical expression at run time. Note that in this case
  *  there is only one such binding, and that is the empty binding.
  */
case class ExpressionNil() extends Expression {
  def getBindings(state: State, binding: Binding) = Iterator.single(binding)
}

/** This class represents an iterator over all the possible bindings that can satisfy an atomic logical expression at run time.
  *  (p t1 t2 ... tn)  -->  an atomic logical expression, where p is a predicate name and each ti is a parameter.
  */
case class ExpressionAtomic(predicateName: String, blueprint: Bindable) extends Expression {
  /** Requesting an iterator over the state will always yield a lazily evaluated iterator. */

  def getBindings(state: State, binding: Binding): Iterator[Binding] = {
    state.getBindings(predicateName, blueprint, binding)
  }
}

/** Reject bindings that fulfill the given precondition. */
case class ExpressionNot(precond: Expression) extends Expression {
  def getBindings(state: State, binding: Binding) = {
    if (precond.getBindings(state, binding).hasNext) Iterator.empty
    else Iterator.single(binding)
  }
}

/**
  * LE is an expression of the form
  * (and [LE1 LE2 ... LEn])
  * where LE1, LE2, ..., and LEn are logical expressions such that each LEi is a consequent of S and X.
  * Note that by definition, an empty logical expression is a consequent of any state and any axiom list.
  */
case class ExpressionAnd(left: Expression, right: Expression) extends Expression {
  /** This returns a lazy iterator over the possible bindings where both preconditions, left and right, are fulfilled. */
  def getBindings(state: State, binding: Binding) = {
    val abc = for {
      leftBinding <- left.getBindings(state, binding)
      rightBinding <- right.getBindings(state, leftBinding)
    } yield {
      rightBinding
    }
    abc
  }
}

case class ExpressionOr(left: Expression, right: Expression) extends Expression {

  def getBindings(state: State, binding: Binding) = {
    val iterators = left.getBindings(state, binding)
    if(iterators.isEmpty) right.getBindings(state, binding)
    else left.getBindings(state, binding) ++ right.getBindings(state, binding)
  }

  //override def toString = left + " or " + right
}

case class ExpressionImply(first: Expression, second: Expression) extends Expression {
  def getBindings(state: State, binding: Binding) = {
    ExpressionOr(ExpressionNot(first), second).getBindings(state, binding)
  }
}

/** Comparison expression interprets its value as true or false */
case class ExpressionComparison(comparison: Comparison) extends Expression {
  def getBindings(state: State, binding: Binding): Iterator[Binding] = {
    val result = comparison.evaluate(state, binding)
    if(result) Iterator.single(binding)
    else Iterator.empty
  }
}

case class ExpressionForall(params: List[Term], firstExpr: Expression, secondExpr: Expression)  extends Expression {
  /** Return an iterator over the incoming binding if for every binding in the "firstExpr"
    * precondition, there exists a binding in the "secondExpr" precondition.
    */

  def getBindings(state: State, binding: Binding): Iterator[Binding] = {
    val iterators = firstExpr.getBindings(state, binding).map(_ filter params.asInstanceOf[List[Var]]).map(secondExpr.getBindings(state, _))
    /** Note: when there are no iterators, it means there are no bindings
      * to begin with which makes the forall precondition evaluate to truth. */
    if (iterators.nonEmpty && iterators.exists(_ isEmpty)) Iterator.empty
    else Iterator.single(binding)
  }
}