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

class PlanTestForSchedules extends FlatSpec with Matchers {
  
  "withSchedule" should "give the same plan with a given schedule" in {
    val t1 = Task('t1, 1.0)
    val t2 = Task('t2, 1.0)
    val p = new Plan {
      val tasks = Set(t1, t2)
      val dependencies = Set(t1 -> t2)
      override lazy val schedule = new Schedule(Map((t1 -> 0), (t2 -> 10.0)))
    }
    
    val sch2 = new Schedule(Map((t1 -> 0.6), (t2 -> 8.0)))
    val p2 = p.withSchedule(sch2)
    p2.schedule.start(t1) should equal (0.6)
    p2.schedule.start(t2) should equal (8.0)
    p2.tasks should equal (Set(t1, t2))
    p2.dependencies should equal (Set(t1 -> t2))
  }
}