package org.pigsaw.ccpm

import scala.util.parsing.combinator.RegexParsers

/**
 * Parse a piece of text to create a `Plan`. Define a task like this:
 * 
 * {{{
 *     t1: "My first task"
 * }}}
 */
class TextParser extends RegexParsers {
  
  private val word = "[a-zA-Z_][0-9a-zA-Z_]*".r
  
  def doubleQuotedString: Parser[String] = "\"[^\"]+\"".r ^^ { _.toString.tail.init }

  def taskID: Parser[Symbol] = word ^^ { Symbol(_) }
  
  def taskDescription: Parser[String] = "\"" ~> """[^"]*""".r <~ "\"" ^^ { _.toString }
  
  def duration: Parser[Double] = ("""[0-9]*\.[0-9]+""".r | """[0-9]+""".r) ^^ { _.toDouble }
  
  def resource: Parser[String] = (word | doubleQuotedString ) ^^ { _.toString }
  
  def taskLine: Parser[Task] = taskID ~ ":" ~ taskDescription ^^ { case (id ~ ":" ~ desc) => Task(id, desc) }
}