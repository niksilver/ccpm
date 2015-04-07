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

  def taskID: Parser[Symbol] = "[a-zA-Z][0-9a-zA-Z]*".r ^^ { Symbol(_) }
  
  def taskDescription: Parser[String] = "\"" ~> """[^"]*""".r <~ "\"" ^^ { _.toString }
  
  def duration: Parser[Double] = ("""[0-9]*\.[0-9]+""".r | """[0-9]+""".r) ^^ { _.toDouble }
  
  def taskLine: Parser[Task] = taskID ~ ":" ~ taskDescription ^^ { case (id ~ ":" ~ desc) => Task(id, desc) }
}