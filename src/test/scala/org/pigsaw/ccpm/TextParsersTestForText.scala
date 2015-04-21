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

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TextParsersTestForText extends FlatSpec with Matchers {

  "apply" should "give an empty plan if there are no lines" in {
    val reader = new TextParsers
    val p = reader("")._1
    p.tasks.size should equal (0)
  }
  
  it should "give a one-task plan if there's just one task" in {
    val reader = new TextParsers
    val p = reader("t0: \"First task\"")._1
    p.tasks.size should equal (1)
    p.task('t0) should equal (Task('t0, "First task", 0, None))
  }
  
  it should "give a two-task plan if there are just two tasks" in {
    val reader = new TextParsers
    val p = reader(
        """t0: "First task" 1.0
          |t1: "Other task" 1.5""".stripMargin)._1
    p.tasks.size should equal (2)
    p.task('t0) should equal (Task('t0, "First task", 1.0, None))
    p.task('t1) should equal (Task('t1, "Other task", 1.5, None))
  }
  
  it should "give tasks in order if there are many of them" in {
    val reader = new TextParsers
    val p = reader(
        """t0: "First task" 1.0
          |t1: "Other task" 1.5
          |t2: "Two" 2.0
          |t3: "Three" 3.0
          |t4: "Four" 4.0""".stripMargin)._1
    p.tasks.size should equal (5)
    p.tasks.toSeq(0) should equal (Task('t0, "First task", 1.0, None))
    p.tasks.toSeq(1) should equal (Task('t1, "Other task", 1.5, None))
    p.tasks.toSeq(2) should equal (Task('t2, "Two", 2.0, None))
    p.tasks.toSeq(3) should equal (Task('t3, "Three", 3.0, None))
    p.tasks.toSeq(4) should equal (Task('t4, "Four", 4.0, None))
  }
  
  it should "correctly report an error at a specified line (1)" in {
    val reader = new TextParsers
    val errors = reader(
        """t0: "First task" 1.0
          |t1: "Second task" 1.5
          |This line is an error
          |# This is a comment""".stripMargin)._2
    errors.size should equal (1)
    errors(0) should equal (Grammar.LineError(3, "Syntax error"))
  }
  
  it should "correctly report an error at a specified line (2 - to avoid faking)" in {
    val reader = new TextParsers
    val errors = reader(
        """t0: "First task" 1.0
          |Something wrong here
          |# This is a comment""".stripMargin)._2
    errors.size should equal (1)
    errors(0) should equal (Grammar.LineError(2, "Syntax error"))
  }
  
  it should "correctly report multiple errors" in {
    val reader = new TextParsers
    val errors = reader(
        """t0: "First task" 1.0
          |Something wrong here
          |...and here!
          |# This is a comment""".stripMargin)._2
    errors should equal (Seq(Grammar.LineError(2, "Syntax error"), Grammar.LineError(3, "Syntax error")))
  }
  
  it should "include dependencies in the plan" in {
    val reader = new TextParsers
    val p = reader(
        """t0: "First task" 1.0
          |t1: "Other task" 1.5
          |t2: "Two" 2.0
          |t0 -> t1
          |t1 -> t2""".stripMargin)._1
    val t0 = p.task('t0)
    val t1 = p.task('t1)
    val t2 = p.task('t2)
    p.dependencies should contain (t0 -> t1)
    p.dependencies should contain (t1 -> t2)
  }
  
  it should "handle multiple dependencies on one line" in {
    val reader = new TextParsers
    val p = reader(
        """t0: "First task" 1.0
          |t1: "Other task" 1.5
          |t2: "Two" 2.0
          |t0 -> t1 -> t2""".stripMargin)._1
    val t0 = p.task('t0)
    val t1 = p.task('t1)
    val t2 = p.task('t2)
    p.dependencies should contain (t0 -> t1)
    p.dependencies should contain (t1 -> t2)
  }
  
  it should "be able to report an unknown task (if first in seq) in a dependency line" in {
    val reader = new TextParsers
    val errors = reader(
        """t0: "First task" 1.0
          |t1: "Other task" 1.5
          |t2: "Two" 2.0
          |t0 -> t1
          |t100 -> t2""".stripMargin)._2
    errors.size should equal (1)
    errors(0) should equal (Grammar.LineError(5, "Unknown task: t100"))
  }
  
  it should "be able to report an unknown task (if second in seq) in a dependency line" in {
    val reader = new TextParsers
    val errors = reader(
        """t0: "First task" 1.0
          |t1: "Other task" 1.5
          |t2: "Two" 2.0
          |t0 -> t1
          |t1 -> t200""".stripMargin)._2
    errors.size should equal (1)
    errors(0) should equal (Grammar.LineError(5, "Unknown task: t200"))
  }
  
  it should "report multiple unknown tasks (if on the same line) in a dependency line" in {
    val reader = new TextParsers
    val errors = reader(
        """t0: "First task" 1.0
          |t1: "Other task" 1.5
          |t2: "Two" 2.0
          |t99 -> t1 -> t200""".stripMargin)._2
    errors.size should equal (2)
    errors(0) should equal (Grammar.LineError(4, "Unknown task: t99"))
    errors(1) should equal (Grammar.LineError(4, "Unknown task: t200"))
  }
  
  it should "handle a task which is not in a dependency" in {
    val reader = new TextParsers
    val p = reader(
        """t0: "Begin" 0.0
        |t1: "First task" 3.0
        |t2: "End" 0.0
        |t3: "Other 3" 3.0
        |t4: "Other 4" 4.0
        |t0 -> t1 -> t2""".stripMargin)._1
    p.tasks.size should equal (5)
  }
  
  it should "report an error for an undeclared resource in a task (1)" in {
    val reader = new TextParsers
    val (p, errors) = reader(
        """resource Alice
          |resource Carol
          |t1: "First task" 1.0 (Alice)
          |t2: "Other task" 1.5 (Bob)
          |t3: "Two" 2.0 (Carol)""".stripMargin)  
    errors.size should equal (1)
    errors(0) should equal (Grammar.LineError(4, "Resource Bob needs to be declared before use"))
  }
  
  it should "report an error for an undeclared resource in a task (2 - to avoid faking)" in {
    val reader = new TextParsers
    val (p, errors) = reader(
        """resource Bob
          |resource Carol
          |t1: "First task" 1.0 (Alice)
          |t2: "Other task" 1.5 (Bob)
          |t3: "Two" 2.0 (Carol)""".stripMargin)  
    errors.size should equal (1)
    errors(0) should equal (Grammar.LineError(3, "Resource Alice needs to be declared before use"))
  }
}