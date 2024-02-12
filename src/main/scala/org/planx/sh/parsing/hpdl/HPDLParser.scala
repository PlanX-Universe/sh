package org.planx.sh.parsing.hpdl

import org.planx.sh.problem.{Constant, Number, Predicate, TaskList, Term, Var}
import org.planx.sh.solving
import org.planx.sh.solving.{Comparison, Expression, ExpressionComparison, ExpressionConversions}

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StdTokenParsers

trait HPDLParser extends StdTokenParsers with ImplicitConversions with ExpressionConversions {
  type Tokens = HPDLTokens
  val lexical = new HPDLLexer
  protected var domain_parser_on = false
  protected var domain_requirements = List[String]()
  protected var problem_requirements = List[String]()
  var dpredicates: List[Predicate] = List()
  var dfunctions: List[solving.Function] = List()
  var dvars = new ListBuffer[Var]()

  lazy val require_def = "(" ~> ":requirements" ~> rep(require_key) <~ ")"

  lazy val require_key = (":strips" ^^ {_ => "S"}
    | ":typing" ^^ {_ => "T"}
    | ":negative-preconditions" ^^ {_ => "NP"}
    | ":disjunctive-preconditions" ^^ {_ => "DP"}
    | ":equality" ^^ {_ => "E"}
    | ":universal-preconditions" ^^ {_ => "UP"}
    | ":fluents" ^^ {_ => "F"}
    | ":numeric-fluents" ^^ {_ => "NF"}
    | ":conditional-effects" ^^ {_ => "CE"}
    | ":derived-predicates" ^^ {_ => "DP"}
    )

  lazy val supertype = "-" ~> name

  lazy val task_list: Parser[TaskList] = ("(" ~> ordering ~ rep(tl) <~ ")" ^^ {
    case o ~ ta => o match {
      case "unordered" => TaskList("unordered", ta)
      case "sequence" => TaskList("sequence", ta)
    }}
      | "(" ~> ")" ^^ {_ => TaskList("sequence", List())}
    )
  lazy val ordering = "sequence" | "unordered"
  lazy val task_atom = "(" ~> name ~ terms <~ ")" ^^ {case n ~ t => Predicate(n, t)}
  lazy val simple_task_atom = "(" ~> name <~ ")" ^^ {case n => Predicate(n)}

  lazy val tl = simple_task_atom | task_atom | task_list

  lazy val predicate_name = name

  lazy val atomic_formula = simple_predicate | proper_predicate | equality_predicate
  lazy val simple_predicate = "(" ~> predicate_name <~ ")" ^^ {case pn => Predicate(pn)}
  lazy val proper_predicate = "(" ~> predicate_name ~ terms <~ ")" ^^ {case pn ~ t => Predicate(pn, t)}
  lazy val equality_predicate = "(" ~> "=" ~> term ~ term <~ ")" ^^ {case t1 ~ t2 => ExpressionComparison(Comparison("=", t1 :: t2 :: List()))}

  lazy val terms: Parser[List[Term]] = rep1(term)
  lazy val term = name ^^ {c => Constant(c)} |
    variable ^^ {v => {
      var r: Var = Var(Symbol("-1"))
      for (dv <- dvars) if(dv.name.equals(Symbol(v))) r = dv

      if(!r.name.equals(Symbol("-1"))) r
      else Var(Symbol(v))
    }} |
    number ^^ {n => Number(n.toDouble)} |
   f_exp_f_head

  lazy val function_term = "(" ~> function_symbol ~ terms <~ ")" ^^ {
    case fs ~ t => solving.Function(fs, t)
  }

  lazy val f_exp_f_head = "(" ~> function_symbol ~ opt(terms) <~ ")" ^^ {
    case fs ~ Some(t) => {
      val f = (for {
        dfunction <- dfunctions
        if(fs.equals(dfunction.name) & t.size == dfunction.args.size)
      } yield dfunction).head

      var typeExpr: List[Expression] = List()
      for(arg <- f.args)
        arg match {
          case a: Var =>
            for(tm <- t) {
              if(f.args.indexOf(arg) == t.indexOf(tm))
                typeExpr = (a._type -> tm) :: typeExpr
            }
        }

      solving.Function(fs, t, f.returnType, typeExpr)
    }
    case fs ~ None => {
      val f = (for {
        dfunction <- dfunctions
        if(fs.equals(dfunction.name) & dfunction.args.size == 0)
      } yield dfunction).head
      solving.Function(fs, List(), f.returnType)
    }
  }

  lazy val function_symbol = name
  lazy val assign_op = "assign" | "scale-up" | "scale-down" | "increase" | "decrease"

  lazy val name = accept("name", { case lexical.IdLit(s) => s.toLowerCase } )//ident
  lazy val variable = accept("string", { case lexical.VarIdLit(s) => s.toLowerCase.substring(1) } )
  lazy val number = accept("real", { case lexical.FloatLit(s) => s.toLowerCase } ) | accept("int", { case lexical.IntLit(s) => s.toLowerCase } )
}