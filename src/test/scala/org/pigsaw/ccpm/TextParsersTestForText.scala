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
    val tp = new TextParsers
    val p = tp("")._1
    p.tasks.size should equal (0)
  }
  
  it should "give a one-task plan if there's just one task" in {
    val parsers = new TextParsers
    val p = parsers("t0: \"First task\"")._1
    p.tasks.size should equal (1)
    p.task('t0) should equal (Task('t0, "First task", 0, None))
  }
  
  it should "give a two-task plan if there are just two tasks" in {
    val parsers = new TextParsers
    val p = parsers(
        """t0: "First task" 1.0
          |t1: "Other task" 1.5""".stripMargin)._1
    p.tasks.size should equal (2)
    p.task('t0) should equal (Task('t0, "First task", 1.0, None))
    p.task('t1) should equal (Task('t1, "Other task", 1.5, None))
  }
}