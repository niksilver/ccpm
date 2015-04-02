package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.util.parsing.combinator._

class TextParsersTest extends FlatSpec with Matchers {

  "taskID" should "parse a task ID of letter and number (1)" in {
    new TextParser with ParserMatchers {
      parseAll(taskID, "t1") should parseAs (Symbol("t1"))
    }
  }

  it should "parse a task ID of letter and number (2 - to avoid faking)" in {
    new TextParser {
      parseAll(taskID, "x2").get should equal (Symbol("x2"))
    }
  }

  it should "parse a task ID of just one letter" in {
    new TextParser {
      parseAll(taskID, "k").get should equal (Symbol("k"))
    }
  }

  it should "parse a task ID of several alphanumerics" in {
    new TextParser {
      parseAll(taskID, "klmno5").get should equal (Symbol("klmno5"))
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
  
  "taskDescription" should "parse something that starts and ends with double quotes" in {
    new TextParser with ParserMatchers {
      parseAll(taskDescription, "\"Hello\"") should parseAs ("Hello")
    }
  }
  
  it should "reject anything which doesn't start with double quotes" in {
    new TextParser {
      parseAll(taskDescription, "Hello\"").successful should equal (false)
    }
  }
  
  it should "reject anything which doesn't end with double quotes" in {
    new TextParser {
      parseAll(taskDescription, "\"My message").successful should equal (false)
    }
  }
  
  it should "reject anything with double quotes in the middle" in {
    new TextParser {
      parseAll(taskDescription, "\"My\"message\"").successful should equal (false)
    }
  }
}