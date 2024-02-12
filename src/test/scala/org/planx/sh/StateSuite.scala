package org.planx.sh

import org.junit._
import Assert._
import org.planx.sh.problem.{Constant, Var}
import org.planx.sh.solving.{Bindable, Binding, State}

class StateSuite {
  @Test def verifyDefaults() = {
    val e = new State
    assertEquals(0, e.size("reachable"))
  }
  
  @Test def verifyAdd() = {
    val e = new State
    e.add("reachable", Array("r1", "r2"))
    assertEquals(1, e.size("reachable"))
    e.add("reachable", Array("r1", "r3"))
    assertEquals(2, e.size("reachable"))
  }
  
  @Test def verifyRemove() {
    val e = new State
    val b = new Bindable(List(Var('a), Var('b)))
    var iter:Iterator[Binding] = null

    e.add("reachable", Array(Constant("r1"), Constant("r2")))
    iter = e.getBindings("reachable", b, Binding())
    assertEquals(1, iter.size)
    
    e.remove("reachable", Array(Constant("r1"), Constant("r2")))
    iter = e.getBindings("reachable",b,Binding())
    assertEquals(0, iter.size)
  }
  
  @Test def verifyEmptyBinding() = {
    val e = new State
    e.add("reachable", Array(Constant("r1"), Constant("r2")))
    e.add("reachable", Array(Constant("r1"), Constant("r3")))

    val b = new Bindable(List(Var('a), Var('b)))
    val iter = e.getBindings("reachable",b,Binding())
    assertEquals(2, iter.size)
  }

  @Test def verifyNonEmptyBinding() = {
    val e = new State
    e.add("reachable", Array(Constant("r1"), Constant("r2")))
    e.add("reachable", Array(Constant("r1"), Constant("r3")))

    val blueprint = new Bindable(List(Var('a), Var('b)))
    val binding = Binding((Var('b), Constant("r2")))
    val iter = e.getBindings("reachable", blueprint, binding)
    assertEquals(1, iter.size)
  }
}