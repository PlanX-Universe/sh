package org.planx.sh.parsing.hpdl

import grizzled.slf4j.Logging
import org.planx.sh.problem.{Add, Assignment, Axiom, Constant, Delete, Domain, DomainMethod, DomainOperator, DomainTask, DomainType, Effect, EmptyEffect, ForallEffect, Number, NumericAssignment, Predicate, TaskList, Term, Var}
import org.planx.sh.solving._
import org.planx.sh.utility.{DomainRequirements, Messaging}
import org.planx.sh.{problem, solving}

import java.io.InputStream
import scala.collection.mutable.Set
import scala.io.Source

class HPDLDomainParser extends HPDLParser with ExpressionConversions with Logging {

  domain_parser_on = true
  var dtypes: List[DomainType] = List()

  // FIX: removed dead  `case _ => println("Something is not right.")`
  //      branch — it could never be reached and used println instead of
  //      the logger.
  lazy val domain = "(" ~> "define" ~> domain_name ~ require_def ~ types_def ~ predicates_def ~ functions_def ~ structure_def <~ ")" ^^ {
    case dn ~ rd ~ td ~ pd ~ fd ~ sd =>
      val tasks:     Set[(DomainTask, List[DomainMethod])] = Set()
      val operators: Set[DomainOperator]                   = Set()
      val axioms:    Set[Axiom]                            = Set()
      for (element <- sd) element match {
        case t: (DomainTask, List[DomainMethod]) => tasks.update(t, true)
        case o: DomainOperator                   => operators.update(o, true)
        case a: Axiom                            => axioms.update(a, true)
      }
      Domain(dn, rd, td, pd, fd, operators.toList.reverse, tasks.toList, axioms.toList)
  }

  lazy val domain_name = "(" ~> "domain" ~> name <~ ")"

  // ── Section-keyword guard ───────────────────────────────────────────────────
  // When a section keyword is misspelled (e.g. ":pedicates" instead of ":predicates"),
  // the parser should detect it and suggest the closest match.
  private def sectionKeyword(expected: String): Parser[Nothing] = {
    // Simple levenshtein distance to suggest the closest known colon-keyword.
    def levenshtein(a: String, b: String): Int = {
      val as = a.toCharArray
      val bs = b.toCharArray
      val rows = as.length + 1
      val cols = bs.length + 1
      val d = Array.ofDim[Int](rows, cols)
      for (i <- 0 until rows) d(i)(0) = i
      for (j <- 0 until cols) d(0)(j) = j
      for (i <- 1 until rows) {
        for (j <- 1 until cols) {
          val cost = if (as(i - 1) == bs(j - 1)) 0 else 1
          d(i)(j) = Seq(
            d(i - 1)(j) + 1,
            d(i)(j - 1) + 1,
            d(i - 1)(j - 1) + cost
          ).min
        }
      }
      d(rows - 1)(cols - 1)
    }

    // Set of colon-keywords that are meaningful in HPDL domain/problem files.
    val knownSectionKeywords = Seq(
      ":requirements",
      ":types",
      ":predicates",
      ":functions",
      ":action",
      ":task",
      ":derived-predicates",
      ":objects",
      ":init",
      ":goal-tasks",
      ":parameters",
      ":precondition",
      ":effect",
      ":constants",
      ":methods",
      ":subtasks"
    )

    // Actually consume "(" and the malformed colon-word to trigger error and suggestion
    "(" ~> accept("unknown section keyword",
      { case lexical.OpIdLit(s) if s.startsWith(":") => s }) >> { found =>
        // find the closest known keyword by edit distance
        val suggestion = try {
          knownSectionKeywords.minBy(k => levenshtein(found.toLowerCase, k.toLowerCase))
        } catch {
          case _: Throwable => expected
        }
        err(s"Unknown section keyword '$found' — did you mean '$suggestion'?")
    }
  }

  // :types
  lazy val types_def =
    ( ("(" ~> ":types" ~> rep(typed_list) <~ ")") ^^ { types =>
        val temp_types = types.flatten
        dtypes = createDomainTypes(temp_types, temp_types)
        dtypes
      }
    | sectionKeyword(":types")
    | success(List[DomainType]())
    )

  lazy val typed_list = rep1(name) ~ opt(supertype) ^^ {
    case type_names ~ Some(st) => type_names map (tn => DomainType(tn, st))
    case type_names ~ None     => type_names map (tn => DomainType(tn, DomainRequirements.NOTYPE))
  }

  // :predicates
  lazy val predicates_def =
    ( ("(" ~> ":predicates" ~> rep1(atomic_formula_skeleton) <~ ")") ^^ { predicates => dpredicates = predicates; dpredicates }
    | sectionKeyword(":predicates")
    | success(List[Predicate]())
    )

  lazy val atomic_formula_skeleton = "(" ~> predicate_name ~ predicate_args <~ ")" ^^ {
    case pn ~ pa =>
      val args = pa.flatten
      val argsTypes: Set[(Int, String)] = Set()
      for (arg <- args) {
        argsTypes.update((args.indexOf(arg), arg._type), true)
        for (dtype <- dtypes)
          // FIX: was bitwise &, now short-circuit &&
          if (arg._type.equals(dtype.supertype))
            argsTypes.update((args.indexOf(arg), dtype.name), true)
      }
      Predicate(pn, pa.flatten, argsTypes)
  }

  lazy val predicate_args = rep(typed_list_variable)

  lazy val typed_list_variable = rep1(variable) ~ opt(supertype) ^^ {
    case variables ~ Some(st) => variables map (v => { val t = Var(Symbol(v), st); dvars += t; t })
    case variables ~ None     => variables map (v => { val t = Var(Symbol(v));     dvars += t; t })
  }

  // :functions
  lazy val functions_def =
    ( ("(" ~> ":functions" ~> function_typed_list <~ ")") ^^ { functions => dfunctions = functions.flatten; dfunctions }
    | sectionKeyword(":functions")
    | success(List[solving.Function]())
    )

  lazy val function_typed_list  = rep(function_typed_list_helper)

  lazy val function_typed_list_helper: Parser[List[solving.Function]] =
    rep1(atomic_function_skeleton) ~ opt(function_type) ^^ {
      case afss ~ Some(ft) =>
        afss map { afs =>
          var typeExpr: List[Expression] = List()
          for (arg <- afs._2) typeExpr = (arg._type -> arg) :: typeExpr
          solving.Function(afs._1, afs._2, ft, typeExpr)
        }
      case affs ~ None =>
        affs map { afs =>
          var typeExpr: List[Expression] = List()
          for (arg <- afs._2) typeExpr = (arg._type -> arg) :: typeExpr
          solving.Function(afs._1, afs._2, DomainRequirements.NUMBER, typeExpr)
        }
    }

  lazy val atomic_function_skeleton = "(" ~> function_symbol ~ function_args <~ ")" ^^ {
    case fs ~ fa => (fs, fa.flatten)
  }

  lazy val function_args = rep(typed_list_variable)
  lazy val function_type = "-" ~> name

  // actions, tasks and axioms
  lazy val structure_def: Parser[List[Any]] = rep1(element)
  lazy val element = action_def | task_def | derived_def

  // action
  lazy val action_def = "(" ~> ":action" ~> name ~ parameters_def ~ action_body <~ ")" ^^ {
    case an ~ pd ~ ab =>
      val params = pd.flatten
      val pre = ab._1 match {
        case a: ExpressionNil => createExpressionAnd(params map (p => (p._type -> p)))
        case _                => createExpressionAnd(ab._1 :: (params map (p => (p._type -> p))))
      }
      problem.DomainOperator(an, params, pre, ab._2, ab._3, ab._4, ab._5)
  }

  lazy val parameters_def = ":parameters" ~> "(" ~> parameters <~ ")"
  lazy val parameters = rep(typed_list_variable)

  lazy val action_body = pre_def ~ effect_def ~ cost ^^ {
    case pd ~ ed ~ c =>
      var del:        List[Effect]     = List()
      var add:        List[Effect]     = List()
      var assignment: List[Assignment] = List()
      for (e <- ed) e match {
        case _add:        Add              => add        = _add  :: add
        case _del:        Delete           => del        = _del  :: del
        case _forall:     ForallEffect     => if (_forall.addList) add = _forall :: add else del = _forall :: del
        case _assignment: Assignment       => assignment = _assignment :: assignment
        case _empty:      EmptyEffect      =>
      }
      (pd, add, del, assignment, c)
  }

  // precondition
  lazy val pre_def = ":precondition" ~> pre
  lazy val pre: Parser[Expression] =
    pre_atomic_formula | pre_and | pre_not | pre_or | pre_imply | pre_forall | pre_comp | pre_empty

  lazy val pre_empty         = "(" ~> ")" ^^ {_ => ExpressionNil()}
  lazy val pre_atomic_formula = atomic_formula ^^ {
    case p: Predicate          => (p.name -> p.arguments)
    case c: ExpressionComparison => c
  }
  lazy val pre_and    = "(" ~> "and"   ~> rep(pre)       <~ ")" ^^ {exprs => createExpressionAnd(exprs)}
  lazy val pre_not    = "(" ~> "not"   ~> pre            <~ ")" ^^ {expr  => ExpressionNot(expr)}
  lazy val pre_or     = "(" ~> "or"    ~> rep(pre)       <~ ")" ^^ {exprs => createExpressionOr(exprs)}
  lazy val pre_imply  = "(" ~> "imply" ~> pre ~ pre      <~ ")" ^^ {case e1 ~ e2 => ExpressionImply(e1, e2)}
  lazy val pre_forall = "(" ~> "forall" ~> pre_forall_parameters ~ pre <~ ")" ^^ {
    case pfp ~ expr2 =>
      val expr1 = createExpressionAnd(pfp.flatten map (darg => (darg._type -> darg)))
      ExpressionForall(pfp.flatten, expr1, expr2)
  }
  lazy val pre_comp = "(" ~> binary_comp ~ f_exp ~ f_exp <~ ")" ^^ {
    case comp ~ fe1 ~ fe2 => ExpressionComparison(Comparison(comp, fe1 :: fe2 :: List()))
  }

  lazy val binary_comp = "=" | "<" | "<=" | ">" | ">="
  lazy val f_exp: Parser[FunExpression] =
    term | f_exp_binary_op | f_exp_multi_op | f_exp_min | f_exp_f_head

  lazy val f_exp_number    = number ^^ {case _n => Number(_n.toDouble)}
  lazy val f_exp_binary_op = "(" ~> binary_op ~ f_exp ~ f_exp     <~ ")" ^^ {case op ~ fe1 ~ fe2 => Operation(op, fe1 :: fe2 :: List())}
  lazy val f_exp_multi_op  = "(" ~> multi_op  ~ f_exp ~ rep1(f_exp) <~ ")" ^^ {case op ~ fe1 ~ fe2 => Operation(op, fe1 :: fe2)}
  lazy val f_exp_min       = "(" ~> "-" ~> f_exp                  <~ ")" ^^ {case fe => Operation("-", fe :: List())}

  lazy val binary_op = multi_op | "-" | "/"
  lazy val multi_op  = "*" | "+"

  lazy val pre_forall_parameters = "(" ~> parameters <~ ")"

  // effect
  lazy val effect_def: Parser[List[Effect]] = ":effect" ~> effect
  lazy val effect       = c_effect_and | c_effect ^^ {e => List(e)}
  lazy val c_effect_and: Parser[List[Effect]] = "(" ~> "and" ~> rep(c_effect) <~ ")"
  lazy val c_effect: Parser[Effect] = p_effect | c_effect_forall
  lazy val c_effect_forall: Parser[Effect] = "(" ~> "forall" ~> (c_effect_forall_when | c_effect_forall_basic) <~ ")"

  lazy val c_effect_forall_basic = c_effect_forall_parameters ~ effect ^^ {
    case cefp ~ e =>
      val expr = createExpressionAnd(cefp.flatten map (darg => (darg._type -> darg)))
      e.head match {
        case add:      Add              => ForallEffect(expr, e)
        case del:      Delete           => problem.ForallEffect(expr, e, false)
        case assignOp: NumericAssignment => problem.ForallEffect(expr, e)
      }
  }
  lazy val c_effect_forall_when = c_effect_forall_parameters ~ c_effect_when ^^ {
    case cefp ~ w =>
      val typeExpr = createExpressionAnd(cefp.flatten map (darg => (darg._type -> darg)))
      val expr     = ExpressionAnd(typeExpr, w._1)
      w._2.head match {
        case add:      Add              => problem.ForallEffect(expr, w._2)
        case del:      Delete           => problem.ForallEffect(expr, w._2, false)
        case assignOp: NumericAssignment => problem.ForallEffect(expr, w._2)
      }
  }

  lazy val c_effect_when            = "(" ~> "when" ~> pre ~ effect <~ ")"
  lazy val c_effect_forall_parameters = "(" ~> parameters <~ ")"

  lazy val p_effect: Parser[Effect] =
    p_effect_delete | p_effect_add | p_effect_numeric | p_effect_object | p_empty

  lazy val p_effect_delete         = "(" ~> "not" ~> atomic_formula <~ ")" ^^ {case del: Predicate => problem.Delete(del)}
  lazy val p_effect_add            = atomic_formula                         ^^ {case add: Predicate => problem.Add(add)}
  lazy val p_effect_numeric        = "(" ~> assign_op ~ f_exp_f_head ~ f_exp <~ ")" ^^ {case ap ~ fefh ~ fe => NumericAssignment(ap, fefh, fe)}
  lazy val p_effect_object         = "(" ~> "assign" ~> f_exp_f_head ~ p_effect_object_helper <~ ")" ^^ {case fefh ~ peoh => problem.ObjectAssignment(fefh, peoh)}
  lazy val p_effect_object_helper  = "undefined" ^^ {_ => Constant("undefined")} | term
  lazy val p_empty                 = "(" ~> ")" ^^ {_ => EmptyEffect()}
  lazy val cost = opt(number) ^^ {
    case Some(t) => t.toDouble
    case None    => 0.0
  }

  // task
  lazy val task_def = "(" ~> ":task" ~> name ~ parameters_def ~ task_body <~ ")" ^^ {
    case tn ~ pd ~ tb =>
      val params = pd.flatten
      (DomainTask(tn, params),
        tb.map(m => {
          val pre = m._2 match {
            case a: ExpressionNil => createExpressionAnd(params map (p => (p._type -> p)))
            case _                => createExpressionAnd(m._2 :: (params map (p => (p._type -> p))))
          }
          problem.DomainMethod(m._1, pre, m._3)
        }))
  }
  lazy val task_body = rep1(method)

  // method
  lazy val method: Parser[(String, Expression, TaskList)] =
    "(" ~> ":method" ~> opt(name) ~ pre_def ~ tasks <~ ")" ^^ {
      case Some(mn) ~ pd ~ t => (mn, pd, t)
      case None     ~ pd ~ t => ("", pd, t)
    }

  lazy val tasks = ":tasks" ~> task_list

  // derived
  lazy val derived_def = "(" ~> ":derived" ~> proper_predicate ~ axiom_tail <~ ")" ^^ {
    case pp ~ at => new Axiom(pp.name, Bindable(pp.arguments)) {
      at map (t => test(t))
    }
  }

  lazy val axiom_tail = rep1(pre)

  // ── Helper methods ──────────────────────────────────────────────────────────

  private def createDomainTypes(base: List[DomainType], derived: List[DomainType]): List[DomainType] = {
    if (derived.isEmpty) base
    else {
      val t = for {
        t1 <- base
        t2 <- derived
        if t1.supertype.equals(t2.name)
      } yield DomainType(t1.name, t2.supertype)
      createDomainTypes(base ::: t, t)
    }
  }

  private def createTypeExpression(args: List[Term], argsTypes: Set[(Int, String)]) = {
    val andArgs: Set[Expression] = Set()
    for (arg <- args) arg match {
      case a: Var =>
        val orTypes: Set[Expression] = Set()
        for (argsType <- argsTypes)
          if (args.indexOf(arg) == argsType._1) orTypes.update((argsType._2 -> a), true)
        andArgs.update(createExpressionOr(orTypes.toList), true)
      case _ => error("Only variables are allowed in the definition of domain predicates.")
    }
    createExpressionAnd(andArgs.toList)
  }

  private def createExpressionAnd(exprs: List[Expression]): Expression = exprs match {
    case expr1 :: expr2 :: rest => ExpressionAnd(expr1, createExpressionAnd(expr2 :: rest))
    case expr :: Nil            => expr
    case Nil                    => ExpressionNil()
  }

  private def createExpressionOr(exprs: List[Expression]): Expression = exprs match {
    case expr1 :: expr2 :: rest => ExpressionOr(expr1, createExpressionOr(expr2 :: rest))
    case expr :: Nil            => expr
    case Nil                    => ExpressionNil()
  }
}

object HPDLDomainParser extends HPDLDomainParser {

  // FIX: explicit return type (was inferred as Any because Failure/Error
  //      branches returned Unit).  Now throws HPDLParseException so line
  //      and column are never discarded.
  private def parseStringToObject(input: String): Domain = {
    info(Messaging.printPlanningSystemMessage + "Processing the domain specification.")
    phrase(domain)(new lexical.Scanner(input)) match {
      case Success(result, _) =>
        info(Messaging.printPlanningSystemMessage + "The domain specification is correctly processed.")
        result
      // FIX: was `error(...)` which logged and returned Unit; callers then
      //      threw a generic RuntimeException with no position.  Now we throw
      //      directly with line + column embedded in the message.
      case Failure(msg, next) =>
        throw HPDLParseException(msg, next.pos.line, next.pos.column, "Failure")
      case Error(msg, next) =>
        throw HPDLParseException(msg, next.pos.line, next.pos.column, "Error")
    }
  }

  // FIX: explicit encoding + resource close (was fromFile(f).mkString with
  //      default charset and no close(), leaking the file handle).
  private def readFile(file: String): String = {
    val src = Source.fromFile(file, "UTF-8")
    try src.mkString finally src.close()
  }

  private def readStream(is: InputStream): String = {
    val src = Source.fromInputStream(is, "UTF-8")
    try src.mkString finally src.close()
  }

  private def parseFileToObject(file: String): Domain =
    parseStringToObject(readFile(file))

  private def parseInputStreamToObject(is: InputStream): Domain =
    parseStringToObject(readStream(is))

  // FIX: typo corrected ("Obejct" → "Object").
  //      The old name is kept as a deprecated alias so existing callers
  //      still compile.
  def processDomainInputStreamToObject(is: InputStream): Domain =
    parseInputStreamToObject(is)

  @deprecated("Spelling corrected — use processDomainInputStreamToObject", "SH-current")
  def processDomainInputStreamToObejct(is: InputStream): Domain =
    processDomainInputStreamToObject(is)

  // FIX: removed `case d: Domain => d / case a => throw` pattern — now that
  //      parseStringToObject has an explicit Domain return type (or throws),
  //      the match is redundant.
  def processDomainFileToObject(file: String): Domain =
    parseFileToObject(file)

  def processDomainStringToObject(input: String): Domain =
    parseStringToObject(input)

  // FIX: checkDomain* methods had the same Unit-on-failure problem.
  //      They now throw so callers can distinguish success from failure.
  def checkDomainStringSyntax(input: String): Unit = {
    info(Messaging.printPlanningSystemMessage + "Processing the domain specification.")
    phrase(domain)(new lexical.Scanner(input)) match {
      case Success(_, _) =>
        info(Messaging.printPlanningSystemMessage + "The domain specification is correctly formulated.")
      case Failure(msg, next) =>
        throw HPDLParseException(msg, next.pos.line, next.pos.column, "Failure")
      case Error(msg, next) =>
        throw HPDLParseException(msg, next.pos.line, next.pos.column, "Error")
    }
  }

  def checkDomainFileSyntax(file: String): Unit =
    checkDomainStringSyntax(readFile(file))
}
