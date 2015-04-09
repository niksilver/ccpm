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

  // The following definitions allow us to more easily assert
  // other outcomes of a parseResult
  //
  //   pr should parseOkay
  //   pr shoulNot parseOkay
  
  class ResultParseOkayMatcher extends Matcher[self.ParseResult[_]] {
    
    def apply(result: self.ParseResult[_]) = {
      MatchResult(
        result.successful,
        result.toString,
        s"$result - parsing was successful")
    }
  }
  
  val parseOkay = new ResultParseOkayMatcher()

}