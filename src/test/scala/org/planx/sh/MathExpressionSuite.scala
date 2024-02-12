package org.planx.sh

import scala.collection.immutable._
import org.junit._
import Assert._
import org.planx.sh.problem.{Constant, Number, Var}
import org.planx.sh.solving.Calculator


import scala.collection.immutable.Range.Double

class MathExpressionSuite {
  @Test def testSum = {
   val value = Calculator.calculate("+", List(Number(1.0), Number(2.0), Number(3.0)))
   assertEquals(Number(6.0), value)
  }
  @Test def testSub = {
    val value = Calculator.calculate("-", List(Number(1.0), Number(2.0), Number(3.0)))
    assertEquals(Number(-4.0), value)
  }
  @Test def testMul = {
    val value = Calculator.calculate("*", List(Number(1.0), Number(2.0), Number(3.0)))
    assertEquals(Number(6.0), value)
  }
  @Test def testDiv = {
    val value = Calculator.calculate("/", List(Number(6.0), Number(2.0), Number(2.0)))
    assertEquals(Number(1.5), value)

    val value2 = Calculator.calculate("/", List(Number(7.0), Number(2.0), Number(4.0), Number(5.0)))
    assertEquals(Number(0.175), value2)
  }
  @Test def testPower = {
    val value = Calculator.calculate("^", List(Number(2.0), Number(4.0)))
    assertEquals(Number(16.0), value)

    val value2 = Calculator.calculate("^", List(Number(2.0), Number(2.0), Number(2.0), Number(2.0)))
    assertEquals(Number(256.0), value2)

    val value3 = Calculator.calculate("^", List(Number(2.0), Number(3.0), Number(2.0), Number(3.0)))
    assertEquals(Number(262144.0), value3)
  }

  @Test def testLess = {
    val value = Calculator.compare("<", List(Number(1.1), Number(2.1), Number(3.1), Number(4.1), Number(5.1)))
    assertEquals(true, value)

    val value2 = Calculator.compare("<", List(Number(3.2), Number(4.2), Number(5.2), Number(2.2), Number(1.1)))
    assertEquals(false, value2)
  }

  @Test def testLessEq = {
    val value = Calculator.compare("<=", List(Number(2.0), Number(2.0), Number(3.0)))
    assertEquals(true, value)

    val value2 = Calculator.compare("<=", List(Number(3.0), Number(4.5), Number(4.5), Number(2.5)))
    assertEquals(false, value2)
  }

  @Test def testGreat = {
    val value = Calculator.compare(">", List(Number(1.0), Number(2.0), Number(3.0)))
    assertEquals(false, value)

    val value2 = Calculator.compare(">", List(Number(3.0), Number(2.0), Number(1.0)))
    assertEquals(true, value2)

    val value3 = Calculator.compare(">", List(Number(3.0), Number(2.0), Number(1.0), Number(3.0), Number(2.0)))
    assertEquals(false, value3)
  }

  @Test def testGreatEq = {
    val value = Calculator.compare(">=", List(Number(4.3), Number(3.5), Number(3.3), Number(3.3), Number(2.5), Number(2.5)))
    assertEquals(true, value)

    val value2 = Calculator.compare(">=", List(Number(4.0), Number(3.5), Number(4.0), Number(4.0), Number(2.5), Number(2.5)))
    assertEquals(false, value2)
  }

  @Test def testEq = {
    val value = Calculator.compare("=", List(Number(1.0), Number(1.0), Number(1.0)))
    assertEquals(true, value)

    val value2 = Calculator.compare("=", List(Constant("aaa"), Constant("aaa"), Constant("aaa")))
    assertEquals(true, value2)

    val value3 = Calculator.compare("=", List(List(Var('a), Var('b), Var('c)), List(Var('a), Var('b), Var('c)), Number(2)))
    assertEquals(false, value3)
  }

  @Test def testNotEq = {
    val value = Calculator.compare("!=", List(Number(1.0), Number(2.0), Number(3.0)))
    assertEquals(true, value)

    val value2 = Calculator.compare("!=", List(Number(1.0), Number(1.0), Number(1.0)))
    assertEquals(false, value2)

    val value3 = Calculator.compare("!=", List(List(Var('a), Var('b), Var('c)), Number(2.0), List(Constant("aaa"), Constant("bb"), Constant("ccccc"))))
    assertEquals(true, value3)
  }
}