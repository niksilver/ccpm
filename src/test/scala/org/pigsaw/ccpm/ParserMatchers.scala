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

  class ResultParsesAsMatcher[T](expected: T) extends Matcher[self.ParseResult[T]] {
    
    def apply(result: self.ParseResult[T]) = {
      MatchResult(
        result.successful && result.get == expected,
        failureMessage(result, expected),
        s"$result was a successful parse of '$expected'")
    }
    
    private def failureMessage(result: self.ParseResult[T], expected: T): String = {
      if (!result.successful) {
        result.toString
      } else {
        s"$result parsed but did not match $expected"
      }
    }
  }

  def parseAs[T](expected: T) = new ResultParsesAsMatcher(expected)
}