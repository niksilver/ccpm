package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.util.parsing.combinator._
import scala.collection.mutable.LinkedHashSet

class TextParsersTest extends FlatSpec with Matchers {
  
  import Grammar._

  "taskID" should "parse a task ID of letter and number (1)" in {
    new TextParser with ParserMatchers {
      parseAll(taskID, "t1") should parseAs (Symbol("t1"))
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
  
  "dependenciesLine" should "parse a single dependency" in {
    new TextParser with ParserMatchers {
      parseAll(dependenciesLine, "t1 -> t2") should parseAs (Set('t1 -> 't2))
      parseAll(dependenciesLine, "k2 -> k1") should parseAs (Set('k2 -> 'k1))
    }
  }
  
  it should "parse multiple dependencies" in {
    new TextParser with ParserMatchers {
      parseAll(dependenciesLine, "t1 -> t2 -> t3 -> t4") should parseAs (Set('t1 -> 't2, 't2 -> 't3, 't3 ->'t4))
      parseAll(dependenciesLine, "k2 -> k1 -> k0") should parseAs (Set('k2 -> 'k1, 'k1 -> 'k0))
    }
  }
  
  it should "reject a single task without a dependency" in {
    new TextParser with ParserMatchers {
      parseAll(dependenciesLine, "t1") shouldNot parseOkay
    }
  }
  
  it should "reject dependencies with trailing arrows" in {
    new TextParser with ParserMatchers {
      parseAll(dependenciesLine, "t1 ->") shouldNot parseOkay
      parseAll(dependenciesLine, "t1 -> t2 ->") shouldNot parseOkay
    }
  }
  
  "resourceDeclaration" should "accept a declared resource" in {
    new TextParser with ParserMatchers {
      parseAll(resourceDeclaration, "resource A") should parseAs (ResourceDeclaration("A"))
      parseAll(resourceDeclaration, "resource Bob") should parseAs (ResourceDeclaration("Bob"))
      parseAll(resourceDeclaration, "resource Charlie34") should parseAs (ResourceDeclaration("Charlie34"))
    }
  }
  
  it should "reject multiple words outside double quotes" in {
    new TextParser with ParserMatchers {
      parseAll(resourceDeclaration, "resource A B") shouldNot parseOkay
      parseAll(resourceDeclaration, "resource Bob,e,brown") shouldNot parseOkay
      parseAll(resourceDeclaration, "resource Charlie-34") shouldNot parseOkay
    }
  }
  
  it should "parse multiple words if they're in double quotes" in {
    new TextParser with ParserMatchers {
      parseAll(resourceDeclaration, """resource "A B"""") should parseAs (ResourceDeclaration("A B"))
      parseAll(resourceDeclaration, """resource "Bob,e,brown"""") should parseAs (ResourceDeclaration("Bob,e,brown"))
      parseAll(resourceDeclaration, """resource "Charlie-34"""") should parseAs (ResourceDeclaration("Charlie-34"))
    }
  }
  
  "comment" should "parse a line that starts with a hash" in {
    new TextParser with ParserMatchers {
      parseAll(comment, "#") should parseAs (Comment())
      parseAll(comment, "# Resource declarations") should parseAs (Comment())
      parseAll(comment, "##---##") should parseAs (Comment())
    }
  }
  
  it should "reject a line that doesn't start with a hash" in {
    new TextParser with ParserMatchers {
      parseAll(comment, "") shouldNot parseOkay
      parseAll(comment, "Resource declarations") shouldNot parseOkay
      parseAll(comment, "--##") shouldNot parseOkay
    }
  }
  
  "line" should "parse a task line" in {
    new TextParser with ParserMatchers {
      parseAll(line, """t0: "Initial" 1.0 (Alice)""").asInstanceOf[ParseResult[TaskLine]] should
          parseAs (TaskLine(Task('t0, "Initial", 1.0, Some("Alice"))))
      parseAll(line, """end: "Final" 2.5 (Bob)""").asInstanceOf[ParseResult[TaskLine]] should
          parseAs (TaskLine(Task('end, "Final", 2.5, Some("Bob"))))
    }
  }
  
  it should "parse a dependencies line" in {
    new TextParser with ParserMatchers {
      parseAll(line, "t1 -> t2").asInstanceOf[ParseResult[DepsLine]] should
          parseAs (DepsLine(Set('t1 -> 't2)))
      parseAll(line, "k2 -> k1 -> k0").asInstanceOf[ParseResult[DepsLine]] should
          parseAs (DepsLine(Set('k2 -> 'k1, 'k1 -> 'k0)))
    }
  }
  
  it should "parse a resource declaration" in {
    new TextParser with ParserMatchers {
      parseAll(line, "resource Bob").asInstanceOf[ParseResult[ResDecLine]] should
          parseAs (ResDecLine("Bob"))
      parseAll(line, """resource "Charlie-34"""").asInstanceOf[ParseResult[ResDecLine]] should
          parseAs (ResDecLine("Charlie-34"))
    }
  }
  
  it should "parse a comment line" in {
    new TextParser with ParserMatchers {
      parseAll(line, "# Resource declarations").asInstanceOf[ParseResult[CommentLine]] should
          parseAs (CommentLine())
      parseAll(line, "##---##").asInstanceOf[ParseResult[CommentLine]] should
          parseAs (CommentLine())
    }
  }

  it should "parse all lines with comments at the end" in {
    new TextParser with ParserMatchers {
      parseAll(line, """t0: "Initial" # First task""").asInstanceOf[ParseResult[TaskLine]] should
          parseAs (TaskLine(Task('t0, "Initial")))
      parseAll(line, "k2 -> k1#My dep here").asInstanceOf[ParseResult[DepsLine]] should
          parseAs (DepsLine(Set('k2 -> 'k1)))
      parseAll(line, """resource "Charlie-34" ###""").asInstanceOf[ParseResult[ResDecLine]] should
          parseAs (ResDecLine("Charlie-34"))
    }
  }
  
  it should "parse a blank line" in {
    new TextParser with ParserMatchers {
      parseAll(line, "").asInstanceOf[ParseResult[BlankLine]] should parseAs (BlankLine())
    }
  }
}