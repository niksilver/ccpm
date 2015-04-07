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
  
  it should "parse a task ID starting with an underscore" in {
    new TextParser with ParserMatchers {
      parseAll(taskID, "_lmno5") should parseAs (Symbol("_lmno5"))
    }
  }
  
  it should "parse a task ID containing an underscore" in {
    new TextParser with ParserMatchers {
      parseAll(taskID, "kl_no5") should parseAs (Symbol("kl_no5"))
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
  
  "duration" should "parse some digits" in {
    new TextParser with ParserMatchers {
      parseAll(duration, "1234") should parseAs (1234.0)
      parseAll(duration, "9") should parseAs (9.0)
    }
  }
  
  it should "match digits dot and digits" in {
    new TextParser with ParserMatchers {
      parseAll(duration, "1234.56") should parseAs (1234.56)
      parseAll(duration, "9.5") should parseAs (9.5)
    }
  }
  
  it should "match just a dot and then digits" in {
    new TextParser with ParserMatchers {
      parseAll(duration, ".56") should parseAs (0.56)
      parseAll(duration, ".5") should parseAs (0.5)
    }
  }
  
  it should "reject no digits after the decimal point" in {
    new TextParser with ParserMatchers {
      parseAll(duration, "1234.") shouldNot parseOkay
      parseAll(duration, "9.") shouldNot parseOkay
    }
  }
  
  it should "reject just a decimal point" in {
    new TextParser with ParserMatchers {
      parseAll(duration, ".") shouldNot parseOkay
    }
  }
  
  it should "reject things with letters in" in {
    new TextParser with ParserMatchers {
      parseAll(duration, "123A.56") shouldNot parseOkay
      parseAll(duration, "1234.5X") shouldNot parseOkay
    }
  }
  
  it should "handle very long numbers okay" in {
    new TextParser with ParserMatchers {
      val num = "123456" + ("0" * 15)
      val out = parseAll(duration, num)
      out should parseOkay
      out.get shouldBe > (123455e15)
      out.get shouldBe < (123457e15)
    }
  }
  
  "resource" should "parse a word okay" in {
    new TextParser with ParserMatchers {
      parseAll(resource, "A") should parseAs ("A")
      parseAll(resource, "Bob") should parseAs ("Bob")
      parseAll(resource, "Charlie34") should parseAs ("Charlie34")
    }
  }
  
  it should "reject a string that starts with a number" in {
    new TextParser with ParserMatchers {
      parseAll(resource, "1Alice") shouldNot parseOkay
    }
  }
  
  it should "parse common characters if we're using double quotes" in {
    def dq(s: String) = '"' + s + '"'
    new TextParser with ParserMatchers {
      parseAll(resource, dq("Alice Cromby")) should parseAs ("Alice Cromby")
      parseAll(resource, dq("Cromby, Alice")) should parseAs ("Cromby, Alice")
      parseAll(resource, dq(" the fin@l frontier")) should parseAs (" the fin@l frontier")
    }
  }
  
  it should "reject a string with a double quote if we're using double quotes" in {
    new TextParser with ParserMatchers {
      parseAll(resource, "\"He\"llo\"") shouldNot parseOkay
    }
  }
  
  "taskLine" should "parse a task ID and description (1)" in {
    new TextParser with ParserMatchers {
      parseAll(taskLine, """t1: "My first task"""") should parseAs (Task('t1, "My first task"))
    }
  }
  
  it should "parse a task ID and description (2 - to avoid faking)" in {
    new TextParser with ParserMatchers {
      parseAll(taskLine, """x17: "Some other task"""") should parseAs (Task('x17, "Some other task"))
    }
  }
  
  it should "parse a line with task ID, description, duration and parenthetical resource" in {
    new TextParser with ParserMatchers {
      parseAll(taskLine, """t0: "Initial task" 1.0 (Alice)""") should parseAs (Task('t0, "Initial task", 1.0, Some("Alice")))
      parseAll(taskLine, """end: "Final task" 2.5 (Bob)""") should parseAs (Task('end, "Final task", 2.5, Some("Bob")))
    }
  }
  
  it should "parse a line with task ID, description and duration but no resource" in {
    new TextParser with ParserMatchers {
      parseAll(taskLine, """t0: "Initial task" 1.0""") should parseAs (Task('t0, "Initial task", 1.0, None))
      parseAll(taskLine, """end: "Final task" 2.5""") should parseAs (Task('end, "Final task", 2.5, None))
    }
  }
  
  it should "parse a line with task ID, description and resource but no duration" in {
    new TextParser with ParserMatchers {
      parseAll(taskLine, """t0: "Initial task" (Alice)""") should parseAs (Task('t0, "Initial task", 0.0, Some("Alice")))
      parseAll(taskLine, """end: "Final task" ("Bob Bobbington")""") should parseAs (Task('end, "Final task", 0.0, Some("Bob Bobbington")))
    }
  }
}