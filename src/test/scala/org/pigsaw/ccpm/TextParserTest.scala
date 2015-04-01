package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.util.parsing.combinator._

class TextParserTest extends FlatSpec with Matchers {

  class TextParser extends RegexParsers {
    def taskID: Parser[Symbol] = "[a-zA-Z][0-9a-zA-Z]".r ^^ { Symbol(_) }
  }

  "taskID" should "parse a task ID of letter and number (1)" in {
    new TextParser {
      parseAll(taskID, "t1").get should equal (Symbol("t1"))
    }
  }

  it should "parse a task ID of letter and number (2 - to avoid faking)" in {
    new TextParser {
      parseAll(taskID, "x2").get should equal (Symbol("x2"))
    }
  }

  it should "reject a string starting with a digit" in {
    new TextParser {
      parseAll(taskID, "34").successful should equal (false)
    }
  }

  it should "reject a string with punctuation" in {
    new TextParser {
      parseAll(taskID, "ty,oi").successful should equal (false)
    }
  }
}