package org.planx.sh

import scala.collection.immutable._
import org.junit._
import org.junit.Assert._
import org.planx.sh.problem.{Constant, Var}
import org.planx.sh.solving.{Bindable, Binding}

class BindableSuite {
  @Test def verifySimple = {
    val b = Bindable(Var('a))
    val binding = b.unify(List(Constant("foo")), Binding())
    assert(binding.isDefined)
    assertEquals(binding.get(Var('a)), Constant("foo"))
  }
  
  @Test def verifyMultiple = {
    val b = Bindable(List(Var('a), Var('b), Var('c)))
    val binding = b.unify(List(Constant("foo"), Constant("bar") ,Constant("qux")), Binding())
    assert(binding.isDefined)
    assertEquals(binding.get(Var('a)), Constant("foo"))
    assertEquals(binding.get(Var('b)), Constant("bar"))
    assertEquals(binding.get(Var('c)), Constant("qux"))
  }
  
  @Test def verifyPrebound = {
    val b = Bindable(List(Constant("a"), Var('b)))
    assertFalse(b.unify(List(Constant("b"), Constant("c")), Binding()).isDefined)
    assertTrue(b.unify(List(Constant("a"), Constant("c")), Binding()).isDefined)
  }
  
  @Test def verifyApply = {
    val b = Bindable(List(Constant("a"), Var('b)))
    assertEquals(b.apply(Binding((Var('c), Constant("foo")))), List(Constant("a"), Var('b)))
    assertEquals(b.apply(Binding((Var('b), Constant("foo")))), List(Constant("a"), Constant("foo")))
  }
  
  @Test def verifyDuplicate = {
    val b = Bindable(List(Var('a), Var('a)))
    val binding = Binding()
    assertFalse(b.unify(List(Constant("foo"), Constant("bar")), binding).isDefined)
    assertTrue(b.unify(List(Constant("foo"), Constant("foo")), binding).isDefined)
  }
  
  @Test def verifyExisting = {
    val b = Bindable(List(Var('a), Var('b)))
    val binding = Binding((Var('a), Constant("foo")))
    assertFalse(b.unify(List(Constant("bar"), Constant("foo")), binding).isDefined)
    assertTrue(b.unify(List(Constant("foo"), Constant("bar")), binding).isDefined)
  }
}