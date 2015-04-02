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
    new TextParser with ParserMatchers {
      parseAll(taskID, "x2") should parseAs (Symbol("x2"))
    }
  }

  it should "parse a task ID of just one letter" in {
    new TextParser with ParserMatchers {
      parseAll(taskID, "k") should parseAs (Symbol("k"))
    }
  }

  it should "parse a task ID of several alphanumerics" in {
    new TextParser with ParserMatchers {
      parseAll(taskID, "klmno5") should parseAs (Symbol("klmno5"))
    }
  }

  it should "reject a string starting with a digit" in {
    new TextParser with ParserMatchers {
      parseAll(taskID, "34") shouldNot parseOkay
    }
  }

  it should "reject a string with punctuation" in {
    new TextParser with ParserMatchers {
      parseAll(taskID, "ty,oi") shouldNot parseOkay
    }
  }
  
  "taskDescription" should "parse something that starts and ends with double quotes" in {
    new TextParser with ParserMatchers {
      parseAll(taskDescription, "\"Hello\"") should parseAs ("Hello")
    }
  }
  
  it should "reject anything which doesn't start with double quotes" in {
    new TextParser with ParserMatchers {
      parseAll(taskDescription, "Hello\"") shouldNot parseOkay
    }
  }
  
  it should "reject anything which doesn't end with double quotes" in {
    new TextParser with ParserMatchers {
      parseAll(taskDescription, "\"My message") shouldNot parseOkay
    }
  }
  
  it should "reject anything with double quotes in the middle" in {
    new TextParser with ParserMatchers {
      parseAll(taskDescription, "\"My\"message\"") shouldNot parseOkay
    }
  }
}