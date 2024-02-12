package org.planx.sh.utility

object Statistics {

  var numberOfPlans = 0
  var planLength = 0
  var parsingTime = 0.0
  var planningTime = 0.0
  var numberOfRecursiveCalls = 0
  var memory = 0.0
  var numberOfEvaluatedStates = 0
  var numberOfDecomposedTasks = 0

  def print = {
    println("-------------------------------------------* Statistics *-------------------------------------------")
    println("Number of found plans: " + numberOfPlans)
    println("First plan length: " + planLength)
    println("*************************************************")
    println("Parsing time: " + parsingTime + " ms")
    println("Plan-generation time: " + planningTime + " ms")
    println("Memory usage: " + memory / 1000000 + " MB")
    println("*************************************************")
    println("Number of evaluated states: " + numberOfEvaluatedStates)
    println("Number of decomposed tasks: " + numberOfDecomposedTasks)
    println("Number of recursions: " + numberOfRecursiveCalls)
    println("----------------------------------------------------------------------------------------------------")
  }
}