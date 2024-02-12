package org.planx.sh.solution

import org.planx.sh.solving.InstanceUnifier

/** Beware: the traversable includes a null element at the end. See the
  * implementation of PlanGeneration for more information.
  */
case class Plan(p: Traversable[InstanceUnifier]) {
  private val steps = p.toList
  val length: Int = steps.length

  def print(): Unit = {
    for ((op, i) <- steps.reverse.zipWithIndex)
      println("%4d) ".format(i+1) + op.name + "(" + op.arguments.mkString(",") + ")")
  }

  override def toString: String = {
    var result = ""
    for ((op, i) <- steps.reverse.zipWithIndex)
      result += (i+1) + ". " + op.name + "(" + op.arguments.mkString(",") + ")" + "\n"
    result
  }
}