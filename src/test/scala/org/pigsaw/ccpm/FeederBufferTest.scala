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

class FeederBufferTest extends FlatSpec with Matchers {

  "FeederBuffer.duration" should "give 0 if no paths" in {
    FeederBuffer.duration(Set()) should equal (0)
  }
  
  it should "give half the length of a single path if there's just one path" in {
    val t1 = Task('t1, 4)
    val t2 = Task('t2, 5)
    val paths = Set(Seq(t1, t2))
    FeederBuffer.duration(paths) should equal ((4.0 + 5.0)/2)
  }
  
  it should "give half the length of the longest path if there are several" in {
    val t1a = Task('t1a, 4)
    val t1b = Task('t1b, 5)
    val t2a = Task('t2a, 3)
    val t2b = Task('t2b, 4)
    val t3a = Task('t3a, 2)
    val t3b = Task('t3b, 8)
    val paths = Set(Seq(t1a, t1b), Seq(t2a, t2b), Seq(t3a, t3b))
    FeederBuffer.duration(paths) should equal ((t3a.duration + t3b.duration)/2)
  }
}