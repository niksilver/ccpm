package org.pigsaw.ccpm

/* Copyright Nik Silver 2015.
 * 
 * This file is part of CCPM.
 *
 * CCPM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *  
 * CCPM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with CCPM.  If not, see <http://www.gnu.org/licenses/>.
 */

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
 * You can inject comments if they start with a `#`. They can
 * appear at the start of a line, or at the end:
 * {{{
 *     # Resource declarations...
 *     resource Alice    # That's Alice
 * }}}
 */
class TextParsers extends RegexParsers {
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
  
  /**
   * Parse a textual plan
   */
  def apply(text: String): Tuple2[Plan, String] = {
    val res = parseAll(line, text)
    val pc = new PlanContext
    res match {
      case Success(TaskLine(t), _) => pc.tasks += t
      case Success(BlankLine(), _) => // Nothing
      case other => throw new NotImplementedError(other.toString)
    }
    val p = new Plan {
      val tasks = pc.tasks
      val dependencies = Set[(Task, Task)]()
    }
    (p, "Hello!")
  }
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