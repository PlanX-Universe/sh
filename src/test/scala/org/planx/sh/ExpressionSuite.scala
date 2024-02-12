package org.planx.sh

import scala.collection.immutable._
import org.junit._
import Assert._
import org.planx.sh.problem.{Constant, Number, Var}
import org.planx.sh.solving.{Binding, Expression, ExpressionConversions, ExpressionNot, Operation, State}

class ExpressionSuite extends ExpressionConversions {
  var state = new State

  /* Create chain of reachable predicates */
  var range = 1 to 8
  for(index <- range){
    if (index < range.end){
      state.add("reachable", Array(Constant(index.toString), Constant((index+1).toString)))
    }
    if (index > range.start){
      state.add("reachable", Array(Constant(index.toString), Constant((index-1).toString)))
    }
  }

  /** The iterator is used everywhere */
  var i: Iterator[Binding] = Iterator.empty

  def extractFromBinding(i: Iterator[Binding], s: Var) = {
    i.toList.map(_(s).toString).sortWith(_<_)
  }

  @Test def testSimpleUnboundTerm = {
    val expr: Expression = "reachable" -> (Var('a), Var('b))
    i = expr.getBindings(state, Binding())
    assertEquals(2*range.length-2, i.size)
  }

  @Test def testSimpleBoundTerm = {
    val expr: Expression = "reachable" -> (Var('a), Var('b))
    i = expr.getBindings(state, Binding((Var('a), Constant("2"))))
    assertEquals(List("1", "3"), extractFromBinding(i, Var('b)))
  }

  @Test def testNotTerm = {
    val expr: Expression = ExpressionNot("reachable" -> (Var('a), Var('b)))

    /** Return empty iterator when there is a possible binding */
    i = expr.getBindings(state, Binding((Var('a), Constant("2"))))
    assertEquals(0, i.size)

    /** Return the binding itself when there is no possible binding */
    i = expr.getBindings(state, Binding((Var('a), Constant("foo"))))
    assertEquals(List("foo"), extractFromBinding(i, Var('a)))
  }

  @Test def testSimpleAndTerm = {
    val expr: Expression = ("reachable" -> (Var('a), Var('b))) & ("reachable" -> (Var('b), Var('c)))
    i = expr.getBindings(state,Binding((Var('a), Constant("1")), (Var('c), Constant("3"))))
    assertEquals(List("2"), extractFromBinding(i, Var('b)))
  }

  @Test def testSimpleOrTerm = {
    val expr: Expression = ("reachable" -> (Var('a), Var('b))) || ("reachable" -> (Var('c), Var('d)))
    // Both predicates are true
    i = expr.getBindings(state,Binding((Var('a), Constant("1")), (Var('d), Constant("2"))))
    assertEquals(3, i.size)

    // Second predicate is false
    i = expr.getBindings(state,Binding((Var('a), Constant("1")), (Var('d), Constant("0"))))
    assertEquals(1, i.size)

    // First predicate is false
    i = expr.getBindings(state,Binding((Var('a), Constant("0")), (Var('d), Constant("2"))))
    assertEquals(2, i.size)

    // Both predicates are false
    i = expr.getBindings(state,Binding((Var('a), Constant("0")), (Var('d), Constant("0"))))
    assertEquals(0, i.size)

    val expr2: Expression = ("reachable" -> (Var('a), Var('b))) || ("reachable" -> (Var('c), Var('d)) || ("reachable" -> (Var('e), Var('f))))
    i = expr2.getBindings(state,Binding((Var('a), Constant("1")), (Var('d), Constant("2")), (Var('f), Constant("6"))))
    assertEquals(5, i.size)
  }

  @Test def testLargeAndTerm = {
    val expr: Expression = (1 to (range.end-1)).map((index) => {
      /** Range starts with 1, so symbols start with 'a. By both mapping
        * to preconditions and reducing with &, the result is a chain of
        * ExpressionAnd terms that looks like: 'a -> 'b, 'b -> 'c, ... */
      ("reachable" -> (
        Var(Symbol(index.toString)),
        Var(Symbol((index+1).toString))))
    } : Expression).reduceLeft(_ & _)

    /** Single possibility to go entirely from front to back */
    i = expr.getBindings(state, Binding((Var(Symbol("1")), Constant("1")), (Var(Symbol("8")), Constant("8"))))
    assertEquals(1, i.size)

    /** It is not possible to go from 1 to 7 in 8 steps, because
      * the predicates state we need 8 steps. */
    i = expr.getBindings(state, Binding((Var(Symbol("1")), Constant("1")), (Var(Symbol("8")), Constant("7"))))
    assertEquals(0, i.size)

    /** To go to 6 in 8 steps allows to do do a single back and forth
      * move per traversed node. */
    i = expr.getBindings(state,Binding((Var(Symbol("1")), Constant("1")), (Var(Symbol("8")), Constant("6"))))
    assertEquals(6, i.size)
  }

  @Test def testLargeOrTerm = {
    val expr: Expression = (1 to (range.end-1)).map((index) => {
      /** Range starts with 1, so symbols start with 'a. By both mapping
        * to preconditions and reducing with ||, the result is a chain of
        * ExpressionOr terms that looks like: 'a -> 'b || 'b -> 'c, ... */
      ("reachable" -> (
        Var(Symbol(index.toString)),
        Var(Symbol((index+1).toString))))
    } : Expression).reduceLeft(_ || _)

    /** Single possibility to go entirely from front to back */
    i = expr.getBindings(state, Binding((Var(Symbol("1")), Constant("0")),
      (Var(Symbol("2")), Constant("0")),
      (Var(Symbol("3")), Constant("0")),
      (Var(Symbol("4")), Constant("4")),
      (Var(Symbol("5")), Constant("5")),
      (Var(Symbol("6")), Constant("6")),
      (Var(Symbol("7")), Constant("0")),
      (Var(Symbol("8")), Constant("0"))))

    assertEquals(2, i.size)

  }

  @Test def testImplyTerm = {
    val expr: Expression = ("reachable" -> (Var('a), Var('b))) imply ("reachable" -> (Var('c), Var('d)))
    // Both predicates exist
    i = expr.getBindings(state,Binding((Var('a), Constant("1")), (Var('d), Constant("2"))))
    assertEquals(2, i.size)

    // Second predicate does not exist
    i = expr.getBindings(state,Binding((Var('a), Constant("1")), (Var('d), Constant("0"))))
    assertEquals(0, i.size)

    // First predicate does not exist
    i = expr.getBindings(state,Binding((Var('a), Constant("0")), (Var('d), Constant("2"))))
    assertEquals(3, i.size)

    // Both predicates do not exist
    i = expr.getBindings(state,Binding((Var('a), Constant("0")), (Var('d), Constant("0"))))
    assertEquals(1, i.size)
  }

  @Test def testComparison = {
    val expr1 = "<" compare List(Var('a), Operation("+", List(Number(1.01), Number(2.0))), Var('d), Var('e))
    i = expr1.getBindings(state, Binding((Var('a), Number(1.0)), (Var('d), Number(3.5)), (Var('e), Number(4.0))))
    assertEquals(1, i.size)

    val expr2 = "<=" compare List(Var('a), Var('b), Var('c), Var('d))
    i = expr2.getBindings(state, Binding((Var('a), Number(2.0)), (Var('b), Number(2.5)), (Var('c), Number(2.5)), (Var('d), Number(3.1))))
    assertEquals(1, i.size)

    val expr7 = "<=" compare List(Var('a), Var('b), Var('c), Var('d))
    i = expr7.getBindings(state, Binding((Var('a), Number(2.0)), (Var('b), Number(2.5)), (Var('c), Number(2.3)), (Var('d), Number(3.1))))
    assertEquals(0, i.size)

    /*val expr4 = "Member" call List(Params(List(Constant("a"), Constant("b"), Constant("c"))), Params(List(Params(List(Constant("a"), Constant("b"), Constant("c"))), Constant("d"), Constant("e"))))
    i = expr4.getBindings(state, Binding((Var('a), Number(29.0))))
    assertEquals(1, i.size)

    val expr5 = "Member" call List(Function("*", List(Number(2.0), Number(2.0))), Number(2.0), Number(3.0), Function("+", List(Number(1.0), Number(3.0))), Number(5.0))
    i = expr5.getBindings(state, Binding((Var('a), Number(29.0))))
    assertEquals(1, i.size)

    val expr6 = "Member" call List(Var('y), Constant("obj2"), Constant("obj3"), Constant("obj4"))
    i = expr6.getBindings(state, Binding((Var('y), Constant("obj4"))))
    assertEquals(1, i.size)*/

  }

}