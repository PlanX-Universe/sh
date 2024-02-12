package org.planx.sh.solving

import org.planx.sh.problem.{Term, Var}
import scala.collection.immutable._

/** Binding - a process of assigning something to something: a name and a value to a variable */
case class Binding(self: Map[Var, Term]) extends Map[Var, Term] {
  def bind(v: Var, value: Term) = {
    get(v) match {
      /** If the variable is already bound to a value, then the new bound value should be equal to the value that was already bound. */
      case Some(_value) => if (_value == value) Option(this) else None
      /** The variable is not yet bound, bind it. */
      case None => Option(Binding(updated(v, value)))
    }
  }
  def update(v: Var, value: Term) = get(v) match {
    /** If the variable is already bound to a value, then the new bound value should be equal to the value that was already bound. */
    case Some(_value) => if (_value == value) Option(this) else Option(Binding(updated(v, value)))
    /** The variable is not yet bound, bind it. */
    case None => Option(Binding(updated(v, value)))
  }

  def filter(names: List[Var]) = Binding(super.filter(names contains _._1))

  private def newProxy[B1 >: Term](newSelf: Map[Var, B1]): Map[Var, B1] =
    new Map[Var, B1] {
      val proxySelf = newSelf

      override def + [B1 >: Term](kv: (Var, B1)): Map[Var, B1] = newProxy(proxySelf + kv).asInstanceOf[Map[Var, B1]]

      override def get(key: Var): Option[B1] = proxySelf.get(key)

      override def iterator: Iterator[(Var, B1)] = proxySelf.iterator

      override def -(key: Var): Map[Var, B1] = proxySelf.-(key)
    }
  override def get(key: Var): Option[Term] = self.get(key)

  override def iterator: Iterator[(Var, Term)] = self.iterator

  override def +[B1 >: Term](kv: (Var, B1)): Map[Var, B1] = newProxy(self + kv)

  override def -(key: Var): Map[Var, Term] = newProxy(self - key)
  override def updated [B1 >: Term](key: Var, value: B1) = newProxy(self.updated(key, value))
}

/** Notice the companion object pattern to circumvent the lack for elaborate overloaded constructor bodies in Scala. */
object Binding {
  def apply(tuples: (Var, Term)*): Binding = Binding(Map() ++ tuples)
}