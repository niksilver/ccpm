package org.pigsaw.ccpm

import scala.util.parsing.combinator.RegexParsers

/**
 * Parse a piece of text to create a `Plan`.
 */
class TextParser extends RegexParsers {

  def taskID: Parser[Symbol] = "[a-zA-Z][0-9a-zA-Z]*".r ^^ { Symbol(_) }
  
  def taskDescription: Parser[String] = "\"" ~> """[^"]*""".r <~ "\"" ^^ { _.toString }
}