package org.planx.sh.parsing.hpdl

import scala.util.matching.Regex
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.token.StdTokens

trait HPDLTokens extends StdTokens {
  // Adapted from StdTokens
  case class IdLit(chars: String) extends Token {
    override def toString = chars
  }
  case class OpIdLit(chars: String) extends Token {
    override def toString = chars
  }
  case class VarIdLit(chars: String) extends Token {
    override def toString = chars
  }
  case class FloatLit(chars: String) extends Token {
    override def toString = chars
  }
  case class IntLit(chars: String) extends Token {
    override def toString = chars
  }
}

class HPDLLexer extends StdLexical with HPDLTokens {
  import scala.util.parsing.input.CharArrayReader.EofCh


  def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      (r findPrefixMatchOf (source.subSequence(offset, source.length))) match {
        case Some(matched) => Success(source.subSequence(offset, offset + matched.end).toString, in.drop(matched.end))
        case None => Failure("String matching regex `" + r + "' expected, but `" + in.first + "' found", in.drop(0))
      }
    }
  }

  reserved ++= List("and",
                    "assign",
                    "increase",
                    "decrease",
                    "scale-up",
                    "scale-down",
                    "define",
                    "domain",
                    "problem",
                    "forall",
                    "imply",
                    "member",
                    "not",
                    "or",
                    "sequence",
                    "unordered",
                    "when")

  delimiters ++= List( "(", ")",
                       ":action",
                       ":domain",
                       ":task",
                       ":tasks",
                       ":method",
                       ":derived",
                       ":parameters",
                       ":precondition",
                       ":effect",
                       ":protected",
                       ":sort-by",
                       ":requirements",
                       ":strips",
                       ":typing",
                       ":negative-preconditions",
                       ":disjunctive-preconditions",
                       ":equality",
                       ":universal-preconditions",
                       ":fluents",
                       ":numeric-fluents",
                       ":derived-predicates",
                       ":predicates",
                       ":functions",
                       ":types",
                       ":objects",
                       ":init",
                       ":goal-tasks",
                       ":conditional-effects",
                       "+", "-", "/", "*", ".", "=", "<", "<=", ">", ">=", "!=", "^")

  override def token: Parser[Token] = {
    (
      regex("""[a-z|A-Z|_][a-z|A-Z|\-|_|?|!|0-9|]*""".r)                            ^^ { case x => process(x) }
        | regex("""[?][a-z|A-Z|\-|_|?|!|0-9|]*""".r)                                ^^ { case x => VarIdLit(x) }
        |regex("""(\+|-)?([0-9]*)(((\.)?[0-9]*(e|E)(\+|-)?[0-9]+)|(\.[0-9]+))""".r) ^^ { case x => FloatLit(x) }
        |regex("""(\+|-)?[0-9]+""".r)                                               ^^ { case x => IntLit(x) }
        | delim
        | regex("""[a-z|A-Z|\-|_|?|!|0-9|]*""".r)                                ^^ { case x => OpIdLit(x) }
        |EofCh ^^^ EOF
      )
  }

  def process(name: String) = if (reserved contains name) Keyword(name) else IdLit(name)

  override def whitespace: Parser[Any] = rep(
    whitespaceChar
      | '/' ~ '*' ~ comment
      | ';' ~ rep(chrExcept('\n', '\r', EofCh))
      | '/' ~ '*' ~> failure("unclosed comment"))

  override protected def comment: Parser[Any] = (
    '*' ~ '/' ^^ { case _ => ' ' }
      | chrExcept(EofCh) ~ comment)

}