package org.planx.sh.solving

import org.planx.sh.problem
import org.planx.sh.problem.Term
import java.lang
import scala.collection.mutable._

/** The class representing the state of the world manipulated by the planner.
  * The state can consist of predicates and (numeric) fluents. Numeric fluents are represented as predicates
  * where the last argument of the fluent is the value of the numeric fluent.
  */
case class State(atoms: Map[String, ArgumentsContainer] = Map()) {
  override def clone: State = State(for((atomName, atomContainer) <- atoms) yield ((atomName, atomContainer.clone)))

  def getContainer(atomName: String) = atoms.get(atomName) match {
    case Some(argumentsContainer) => argumentsContainer
    case None => {
      val argumentsContainer = ArgumentsContainer()
      atoms.update(atomName, argumentsContainer)
      argumentsContainer
    }
  }

  /** Add arguments to the container of a given predicate name. */
  def add[T <: Any](predicateName: String, arguments: Array[T]) = getContainer(predicateName) += arguments.toList

  /** Remove arguments from the container of a given predicate name. */
  def remove[T <: Any](predicateName: String, arguments: Array[T]) = getContainer(predicateName) -= arguments.toList

  def update[T <: Any](functionName: String, arguments: Array[T], value: Any) = {
    getContainer(functionName).update(arguments.toList, value)
  }

  /** Return number of atoms for a given atom name. */
  def size(atomName: String): Int = getContainer(atomName).size

  def getBindings(predicateName: String, blueprint: Bindable, binding: Binding): Iterator[Binding] = {
    val argumentsContainer = getContainer(predicateName)
    val arity = blueprint.arity
    var currentArguments: List[List[Any]] = argumentsContainer.byarity.getOrElse(arity, List())

    /** Find the smallest list of candidates */
    for (
      (argument, i) <- blueprint.apply(binding).zipWithIndex
      if !argument.isInstanceOf[Symbol]) {
      argumentsContainer.byarguments.get((arity, i, argument)) match {
        case Some(subsetOfArguments) => if (subsetOfArguments.length < currentArguments.length) currentArguments = subsetOfArguments
        case _ => ; /** skip */
      }
    }

    new Iterator[Binding] {
      private val _iterator = currentArguments.iterator
      private var _binding: Option[Binding] = None

      def hasNext = { scan; _binding.isDefined }
      def next = { val _r = _binding.get; _binding = None; _r }
      def scan = {
        while(_iterator.hasNext && _binding.isEmpty) {
          _binding = blueprint.unify(_iterator.next, binding)
          _binding
        }
      }
    }
  }

  def evaluate(functionName: String, arguments: List[Term], binding: Binding): problem.Number = {
    val argumentsContainer = getContainer(functionName)
    val arity = arguments.length + 1
    var currentArguments: List[List[Any]] = argumentsContainer.byarity.getOrElse(arity, List())

    /** Find the smallest list of candidates */
    for (
      (argument, i) <- Bindable(arguments).apply(binding).zipWithIndex
      if !argument.isInstanceOf[Symbol]) {
      argumentsContainer.byarguments.get((arity, i, argument)) match {
        case Some(subsetOfArguments) => if (subsetOfArguments.length < currentArguments.length) currentArguments = subsetOfArguments
        case _ => ; /** skip */
      }
    }
    currentArguments match {
      case Nil => throw new RuntimeException("The function cannot associate a value for the given parameters." + (functionName, arguments, binding))
      case _ => currentArguments.head.last.asInstanceOf[problem.Number]
    }
  }
}

/** To provide quick access to atoms in the state, this class keeps
  * a map of (arity, index, argument) to the list of atoms that have this
  * property. This way, it is possible to look up the list of atoms where a
  * specific argument is already provided, to minimize the number of bindings
  * that need to be checked when iterating over the possible bindings.
  * Arity - the number of arguments in a predicate
  */
protected case class ArgumentsContainer(byarity: Map[Int, List[List[Any]]] = Map(), byarguments: Map[(Int, Int, Any), List[List[Any]]] = Map()){
  /** Return the number of different predicates contained. */
  def size: Int = byarity.values.map(_.length).foldLeft(0)(_+_)

  /** Return deep clone of this container. */
  override def clone: ArgumentsContainer = ArgumentsContainer(byarity.clone, byarguments.clone)

  /** Add arguments to the current container
  * 1. Update the lookup map by adding the property (arity, index, argument)
       mapped to the list of atoms that fulfill this property, for the given atom.
    2. Add the arguments to the map that holds a list of arguments by arity.
  */
  def +=(arguments: List[Any]): ArgumentsContainer = {
    byarguments ++= lookForTriples(arguments).map(triple => byarguments.get(triple) match {
      case Some(current) => ((triple, arguments :: current))
      case None => ((triple, List(arguments)))
    })
    byarity += (byarity.get(arguments.length) match {
      case Some(current) => ((arguments.length, arguments :: current))
      case None => ((arguments.length, List(arguments)))
    })
    this
  }

  /** Remove arguments from the current container */
  def -=(arguments: List[Any]): ArgumentsContainer = {

    for (triples <- lookForTriples(arguments)) byarguments.get(triples) match {
      case Some(List(args)) => byarguments.remove(triples)
      case Some(current) => byarguments.update(triples, current filterNot (_ == arguments))
      case None => ;
    }

    byarity.get(arguments.length) match {
      case Some(List(args)) => byarity.remove(args.length)
      case Some(current) => byarity.update(arguments.length, current filterNot (_ == arguments))
      case None => ;
    }
    this
  }

  /** Update the last argument for the current container. The last argument represents the value of a function. */
  def update(arguments: List[Any], value: Any) = {
    for (triple <- lookForTriples(arguments)) byarguments.get(triple) match {
      case Some(List(args)) => {
        if(triple._2 == arguments.length - 1){
          byarguments.get((triple._1, triple._2, value)) match {
            case Some(l) => byarguments.update((triple._1, triple._2, value), (arguments.dropRight(1) :+ value) :: l)
            case None => byarguments.update((triple._1, triple._2, value), List((arguments.dropRight(1) :+ value)))
          }
          byarguments.remove(triple)
        } else byarguments.update(triple, List((arguments.dropRight(1) :+ value)))
      }
      case Some(current) => {
        if(triple._2 == arguments.length - 1) {
          val dr = List((arguments.dropRight(1) :+ value))
          byarguments.get((triple._1,triple._2, value)) match {
            case Some(l) => byarguments.update((triple._1,triple._2, value), dr :: l)
            case None => byarguments.update((triple._1,triple._2, value), dr)
          }
          byarguments.update(triple, current filterNot (_ == arguments))
        } else byarguments.update(triple, (arguments.dropRight(1) :+ value) :: (current filterNot (_ == arguments)))
      }
      case None => ;
    }
    byarity.get(arguments.length) match {
      case Some(List(args)) => byarity.update(arguments.length, (arguments.dropRight(1) :+ value) :: (List(args) filterNot (_ == arguments)))
      case Some(current) => byarity.update(arguments.length, (arguments.dropRight(1) :+ value) :: (current filterNot (_ == arguments)))
      case None => ;
    }
    this
  }

  protected def lookForTriples(arguments: List[Any]): List[(Int, Int, Any)] =
    for ((t, i) <- arguments.zipWithIndex) yield (arguments.length, i, t)

  def mkString = (byarity mkString "---")+ "|===|" + (byarguments mkString "---")
}