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

class ChainTest extends FlatSpec with Matchers {

  "toSeq" should "extract the sequence" in {
    val s = Seq(Task('t1), Task('t2), Task('t3))
    val chain = Chain(s)
    chain.toSeq should equal (s)
  }

  "length" should "give the total length of all durations in the chain (1)" in {
    val t1 = new Task('t1, "My task", 2, Some("Alice"))
    val t2 = new Task('t2, "Task 2", 3, Some("Bob"))
    val t3 = new Task('t3, "Task 3", 4, Some("Carol"))

    val chain = Chain(Seq(t1, t2, t3))
    chain.length should equal (2+3+4)
  }

  it should "give the total length of all durations in the chain (2 - to avoid faking)" in {
    val t1 = new Task('t1, "My task", 5, Some("Alice"))
    val t2 = new Task('t2, "Task 2", 4, Some("Bob"))
    val t3 = new Task('t3, "Task 3", 3, Some("Carol"))

    val chain = Chain(Seq(t1, t2, t3))
    chain.length should equal (5+4+3)
  }
  
  it should "give zero if the chain is empty" in {
    Chain(Nil).length should equal (0)
  }
  
  it should "give the task duration if there's only one task" in {
    val t0 = new Task('t0, "Only task", 5, Some("Alice"))

    val chain = Chain(Seq(t0))
    chain.length should equal (5)
  }
}