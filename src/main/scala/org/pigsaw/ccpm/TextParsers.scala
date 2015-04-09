package org.pigsaw.ccpm

import scala.util.parsing.combinator.RegexParsers
import scala.language.existentials

/**
 * Parse a piece of text to create a `Plan`. Define a task like this:
 *
 * {{{
 *     t1: "My first task"
 *     t2: "Task of duration 3" 3.0
 *     ajob: "Alice's job" 1.5 (Alice)
 *     b76a: "Bob's job" 2.0 ("Bob K")
 *     end: "Carly's task requiring no time" (Carly)
 * }}}
 *
 * But resources need to be declared first like this:
 * {{{
 *     resource Alice
 *     resource "Bob K"
 * }}}
 *
 * Dependencies can be explained like this:
 * {{{
 *     t1 -> t2
 *     ajob -> b76a -> end
 * }}}
 *
 * You can inject comments if they start with a `#`:
 * {{{
 *     # Resource declarations...
 * }}}
 */
class TextParser extends RegexParsers {
  import Grammar._

  private val word = "[a-zA-Z_][0-9a-zA-Z_]*".r

  private val depArrow = "->"

  def doubleQuotedString: Parser[String] = "\"[^\"]+\"".r ^^ { _.toString.tail.init }

  def taskID: Parser[Symbol] = word ^^ { Symbol(_) }

  def taskDescription: Parser[String] = doubleQuotedString

  def duration: Parser[Double] = ("""[0-9]*\.[0-9]+""".r | """[0-9]+""".r) ^^ { _.toDouble }

  def resource: Parser[String] = (word | doubleQuotedString) ^^ { _.toString }

  def taskLine: Parser[Task] =
    taskID ~ ":" ~ taskDescription ~ opt(duration) ~ opt("(" ~> resource <~ ")") ^^
      { case (id ~ ":" ~ desc ~ dur ~ res) => Task(id, desc, dur.getOrElse(0), res) }

  // Convert a List(a, b, c, d) to a Set(a -> b, b -> c, c -> d)
  private def listToPairs(ts: List[Symbol]): Set[(Symbol, Symbol)] =
    (ts.sliding(2) map { p => p(0) -> p(1) }).toSet

  def dependenciesLine: Parser[Set[(Symbol, Symbol)]] =
    taskID ~ depArrow ~ rep1sep(taskID, depArrow) ^^
      { case (t1 ~ depArrow ~ ts) => listToPairs(t1 :: ts) }

  def resourceDeclaration: Parser[ResourceDeclaration] =
    "resource" ~> resource ^^ { ResourceDeclaration(_) }

  def comment: Parser[Comment] = "#" ~> ".*".r ^^ { _ => Comment() }

  def simpleLine: Parser[Line] =
    (taskLine ^^ { TaskLine(_) }) |
      (dependenciesLine ^^ { DepsLine(_) }) |
      (resourceDeclaration ^^ { rd => ResDecLine(rd.name) }) |
      (comment ^^ { _ => CommentLine() }) |
      ("" ^^ { _ => BlankLine() })

  def line: Parser[Line] = simpleLine ~ opt(comment) ^^ { case ln ~ comm => ln }
}

/**
 * Structural elements for a textual plan, produced by the `TextParsers`.
 */
object Grammar {

  /**
   * Declaration of a resource while parsing a textual plan.
   */
  case class ResourceDeclaration(name: String)

  /**
   * A comment in a textual plan.
   */
  case class Comment()

  sealed abstract class Line

  case class TaskLine(t: Task) extends Line
  case class DepsLine(ds: Set[(Symbol, Symbol)]) extends Line
  case class ResDecLine(r: String) extends Line
  case class CommentLine() extends Line
  case class BlankLine() extends Line
}