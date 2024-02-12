package org.planx.sh.problem

import org.planx.sh.utility.Messaging
import org.planx.sh.solving._

abstract class Axiom(name: String, generalArguments: Bindable) extends Expression with ExpressionConversions {
  var n = name
  val specificArguments: Bindable = null
  protected var preconds: List[Expression] = List()
  def preconditions = preconds

  def getBindings(state: State, binding: Binding): Iterator[Binding] = {
    /** Check that generalArguments override the specificArguments. */
    if (specificArguments == null) throw new RuntimeException(Messaging.printPlanningSystemComponentMessage("Axiom") + "Cannot use axiom without #apply!")

    /** Apply binding to specificArguments to get arguments that can be unified with the generalArguments. */
    generalArguments.unify(specificArguments.apply(binding), Binding()) match {
      case Some(_binding) => {
        val defined = (for (pre <- preconditions.iterator) yield pre.getBindings(state, _binding)).find(!_.isEmpty).isDefined
        if (defined) Iterator.single(binding)
        else Iterator.empty
      }
      case _ => Iterator.empty
    }
  }

  def test(expr: Expression) = preconds :+= expr

  def apply(specArguments: Bindable) = {
    val parent = this
    new Axiom(this.n, generalArguments) {
      override val specificArguments: Bindable = specArguments
      override def preconditions = parent.preconds
    }
  }

  def apply(t1: Term): Axiom = apply(Bindable(List(t1)))
  def apply(t1: Term, t2: Term): Axiom = apply(Bindable(List(t1, t2)))
  def apply(t1: Term, t2: Term, t3: Term): Axiom = apply(Bindable(List(t1, t2, t3)))
  def apply(t1: Term, t2: Term, t3: Term, t4: Term): Axiom = apply(Bindable(List(t1, t2, t3, t4)))
}