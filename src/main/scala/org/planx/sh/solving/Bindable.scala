package org.planx.sh.solving

import org.planx.sh.problem.{Term, Value, Var}

import scala.collection.immutable._
import scala.language.implicitConversions

case class Bindable(arguments: List[Term]) {
  val arity = arguments.length

  /** Unifies the parameters with actual values */
  def unify(other: List[Any], in: Binding): Option[Binding] = {
    /** Impossible to unify when the arrays have unequal size. */
    if (other.length != arguments.length) return None
    /** Unify every pair. */
    val abc =arguments.zip(other).foldLeft(Option(in))({
      case (Some(binding), tuple) => tuple match {
        case (variable: Var, value: Term) => binding.bind(variable, value)
        /** For non-symbols in the bindable list, the argument to unify with should match the bindable argument. */
        case _ => if (tuple._1 == tuple._2) Some(binding) else {println("here");None}
      }
      case _ => None
    })
    abc
  }

  def isGround = arguments forall ({
    case v: Var => v.value != null
    case _ => true
  })

  def apply(binding: Binding): List[Term] = arguments.map((argument: Term) => argument match {
    case v: Var => {
      binding.get(v) match {
      case Some(value) => value
      case None => v // TODO: This is not correct
    }
    }
    case value: Value => value
  })
}

/** This object is called whenever Bindable('something) is created */
object Bindable {
  def apply(vars: Var*): Bindable = Bindable(vars.toList)
}

trait BindableConversions {
  implicit def symbolToBindable(t: Term): Bindable = Bindable(List(t))

  private def flatProduct(product: Product): Iterator[Term] = product.productIterator.flatMap {
    case t: Term => Iterator(t)
    case p: Product => flatProduct(p)
  }

  implicit def tuple2ToBindable(t: (Term, Term)): Bindable = Bindable(flatProduct(t).toList)
  implicit def tuple3ToBindable(t: (Term, Term, Term)): Bindable = Bindable(flatProduct(t).toList)
  implicit def tuple4ToBindable(t: (Term, Term, Term, Term)): Bindable = Bindable(flatProduct(t).toList)
  implicit def tuple5ToBindable(t: (Term, Term, Term, Term, Term)): Bindable = Bindable(flatProduct(t).toList)
  implicit def tuple6ToBindable(t: (Term, Term, Term, Term, Term, Term)): Bindable = Bindable(flatProduct(t).toList)
}