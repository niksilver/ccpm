package org.pigsaw.ccpm

import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import scala.util.parsing.combinator.Parsers

import scala.language.existentials

/**
 * Scalatest `Matchers` that are useful for the parsing.
 */
trait ParserMatchers {

  // The following definitions allow us to more easily assert
  // the outcomes of a parseResult
  //
  //   pr should parseAs "Hello"
  //
  // See http://www.scalatest.org/user_guide/using_matchers#usingCustomMatchers

  class ResultParsesAsMatcher[T <: p.ParseResult[U] forSome { type U; val p: Parsers; }](expectedOutput: String, parsers: Parsers) extends Matcher[T] {

    def apply(left: T) = {
      MatchResult(
        left.successful && left.get == expectedOutput,
        left.toString,
        s"$left was a successful parse of '$expectedOutput'")
    }
  }

  def parseAs(expectedOutput: String)(implicit parsers: TextParser) =
    new ResultParsesAsMatcher[parsers.ParseResult[String]](expectedOutput, parsers)

}