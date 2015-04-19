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
import scala.util.parsing.input.Reader
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.CharSequenceReader
import scala.annotation.tailrec

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
class TextParsers extends RegexParsers with PackratParsers {
  import Grammar._

  private val word = "[a-zA-Z_][0-9a-zA-Z_]*".r
  
  private val depArrow = "->"

  lazy val doubleQuotedString: PackratParser[String] = "\"[^\"]+\"".r ^^ { _.toString.tail.init }

  lazy val nonBlankSeq: PackratParser[String] = """\S.*""".r ^^ { _.toString }

  lazy val taskID: PackratParser[Symbol] = word ^^ { Symbol(_) }

  lazy val taskDescription: PackratParser[String] = doubleQuotedString

  lazy val duration: PackratParser[Double] = ("""[0-9]*\.[0-9]+""".r | """[0-9]+""".r) ^^ { _.toDouble }

  lazy val resource: PackratParser[String] = (word | doubleQuotedString) ^^ { _.toString }

  lazy val taskLine: PackratParser[Task] =
    taskID ~ ":" ~ taskDescription ~ opt(duration) ~ opt("(" ~> resource <~ ")") ^^
      { case (id ~ ":" ~ desc ~ dur ~ res) => Task(id, desc, dur.getOrElse(0), res) }

  // Convert a List(a, b, c, d) to a Set(a -> b, b -> c, c -> d)
  private def listToPairs(ts: List[Symbol]): Set[(Symbol, Symbol)] =
    (ts.sliding(2) map { p => p(0) -> p(1) }).toSet

  lazy val dependenciesLine: PackratParser[Set[(Symbol, Symbol)]] =
    taskID ~ depArrow ~ rep1sep(taskID, depArrow) ^^
      { case (t1 ~ depArrow ~ ts) => listToPairs(t1 :: ts) }

  lazy val resourceDeclaration: PackratParser[ResourceDeclaration] =
    "resource" ~> resource ^^ { ResourceDeclaration(_) }

  lazy val comment: PackratParser[Comment] = "#" ~> ".*".r ^^ { _ => Comment() }

  lazy val simpleLine: PackratParser[Line] =
    (taskLine ^^ { TaskLine(_) }) |
      (dependenciesLine ^^ { DepsLine(_) }) |
      (resourceDeclaration ^^ { rd => ResDecLine(rd.name) }) |
      (comment ^^ { _ => CommentLine() })

  lazy val line: PackratParser[Line] =
    (simpleLine ~ opt(comment) ^^ { case ln ~ comm => ln }) |
    (nonBlankSeq ^^ { case ln => BadLine(ln) }) |
    ("" ^^ { _ => BlankLine() })

  def parseLines(lines: String): Seq[Line] = {
    lines.split("\u000a") map { parseAll(line, _).get }
  }
  
  def parseAll[T](fn: Parser[T], text: String): ParseResult[T] =
    parseAll(fn, new PackratReader(new CharSequenceReader(text)))
  
  /**
   * Parse a textual plan
   */
  def apply(text: String): (Plan, Seq[LineError]) = {
    var rs = Set[String]()
    var ts = Seq[Task]()
    var ds = Set[(Task, Task)]()
    var es = Seq[LineError]()
    var num = 0
    
    def task(s: Symbol): Option[Task] = ts find { _.id == s }
    
    def addTask(t: Task) = {
      if (t.resource.nonEmpty && !rs.contains(t.resource.get)) {
        val r = t.resource.get
        es = es :+ LineError(num, s"Resource ${r} needs to be declared before use")
      } else {
        ts = ts :+ t
      }
    }
    
    def addDep(ss: (Symbol, Symbol)) = {
      val t1 = task(ss._1)
      val t2 = task(ss._2)
      if (t1 == None) { es = es :+ LineError(num, "Unknown task: "+(ss._1.name)) }
      if (t2 == None) { es = es :+ LineError(num, "Unknown task: "+(ss._2.name)) }
      if (t1.nonEmpty && t2.nonEmpty) {
        ds = ds + Tuple2(task(ss._1).get, task(ss._2).get)
      }
    }
    
    parseLines(text) foreach { ln =>
      num += 1
      ln match {
        case ResDecLine(r) => rs = rs + r
        case TaskLine(t) => addTask(t)
        case DepsLine(syms) => syms foreach { addDep(_) }
        case BadLine(txt) => es = es :+ LineError(num, "Syntax error")
        case _ => // Ignore
    }}
    val p = new Plan {
      val tasks = ts
      val dependencies = ds
    }
    (p, es)
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

  /**
   * The result of a parsed line.
   */
  sealed abstract class Line

  case class TaskLine(t: Task) extends Line
  case class DepsLine(ds: Set[(Symbol, Symbol)]) extends Line
  case class ResDecLine(r: String) extends Line
  case class CommentLine() extends Line
  case class BlankLine() extends Line
  case class BadLine(line: String) extends Line
  
  /**
   * A report of a line which cannot be parsed.
   * @param num  Line number of the error (starting at 1).
   * @param msg  Error message.
   */
  case class LineError(num: Int, msg: String)
}