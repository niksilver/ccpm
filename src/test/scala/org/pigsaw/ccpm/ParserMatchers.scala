package org.pigsaw.ccpm

import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import scala.util.parsing.combinator.Parsers

import scala.language.existentials

/**
 * Scalatest `Matchers` that are useful for the parsing.
 */
trait ParserMatchers {
  
  self: Parsers =>

  // The following definitions allow us to more easily assert
  // the outcomes of a parseResult
  //
  //   pr should parseAs "Hello"
  //
  // See http://www.scalatest.org/user_guide/using_matchers#usingCustomMatchers

  class ResultParsesAsMatcher(expected: String) extends Matcher[self.ParseResult[String]] {
    
    def apply(result: self.ParseResult[String]) = {
      MatchResult(
        result.successful && result.get == expected,
        result.toString,
        s"$result was a successful parse of '$expected'")
    }
  }

  def parseAs(expected: String) = new ResultParsesAsMatcher(expected)
}