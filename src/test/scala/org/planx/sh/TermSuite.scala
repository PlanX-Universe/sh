package org.planx.sh

import org.junit.Test
import org.junit.Assert._
import org.planx.sh.problem.{Constant, Number, Var}
import org.planx.sh.solving.{Bindable, Binding}

class TermSuite {
  @Test def verifyNumber = {
    val number = Number(2.0)

    val bindable = Bindable(List(number))
    assertTrue(bindable.isGround)

    val binding = bindable.unify(List(Constant("foo")), Binding())
    assert(!binding.isDefined)
  }

  @Test def verifyConstant = {
    val constant = Constant("empty")

    val bindable = Bindable(List(constant))
    assertTrue(bindable.isGround)

    val binding = bindable.unify(List(Constant("foo")), Binding())
    assert(!binding.isDefined)
  }

  @Test def verifyVar = {
    val variable = Var('a)

    val bindable = Bindable(List(variable))
    assertFalse(bindable.isGround)

    val con = Constant("foo")
    val binding = bindable.unify(List(con), Binding())

    assertEquals(binding.get(Var('a)), con)
    //assertTrue(bindable.isGround)
    assert(binding.isDefined)
  }

  @Test def verifyVars = {
    val variable1 = Var('a)
    val variable2 = Var('b)
    val variable3 = Var('c)

    val bindable = Bindable(List(variable1, variable2, variable3))
    assertFalse(bindable.isGround)

    val con1 = Constant("foo")
    val con2 = Number(2.0)
    val con3 = Constant("bbb")

    val binding = bindable.unify(List(con1, con2, con3), Binding())

    assertEquals(binding.get(Var('a)), con1)
    assertEquals(binding.get(Var('b)), con2)
    assertEquals(binding.get(Var('c)), con3)

    assert(binding.isDefined)
    //assertTrue(bindable.isGround)
  }

  @Test def verifyPrebound = {
    val ac = Constant("a")
    val bv = Var('b)

    val bindable = Bindable(List(ac, bv))

    val bc = Constant("b")
    val cc = Constant("c")

    assertFalse(bindable.unify(List(bc, cc), Binding()).isDefined)
    assertTrue(bindable.unify(List(ac, cc), Binding()).isDefined)
  }

  @Test def verifyApply = {
    val ac = Constant("a")
    val bv = Var('b)

    val bindable = Bindable(List(ac, bv))

    val foo = Constant("foo")
    val cv = Var('c)

    assertEquals(bindable.apply(Binding((cv, foo))), List(ac, bv))
    assertEquals(bindable.apply(Binding((bv, foo))), List(ac, foo))
  }

  @Test def verifyDuplicate = {
    val av = Var('a)
    val foo = Constant("foo")
    val bar = Constant("bar")

    val bindable = Bindable(List(av, av))
    val binding = Binding()

    assertFalse(bindable.unify(List(foo, bar), binding).isDefined)
    assertTrue(bindable.unify(List(foo, foo), binding).isDefined)
  }

  @Test def verifyExisting = {
    val av = Var('a)
    val bv = Var('b)

    val bindable = Bindable(List(av, bv))

    val foo = Constant("foo")
    val bar = Constant("bar")

    val binding = Binding((av, foo))

    assertFalse(bindable.unify(List(bar, foo), binding).isDefined)
    assertTrue(bindable.unify(List(foo, bar), binding).isDefined)
  }
}