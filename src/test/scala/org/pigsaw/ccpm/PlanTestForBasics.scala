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
import org.scalatest.Ignore

class PlanTestForBasics extends FlatSpec with Matchers with ScheduleMatchers {
  
  "tasks" should "return all tasks in order" in {
    val ts = (0 to 99) map { i => Task(Symbol("t"+i), "Task #"+i, i, None) }
    val p = new Plan {
      val tasks = ts
      val dependencies = Set[(Task, Task)]()
    }
    (0 to 99).foreach { i =>
      p.tasks.toSeq(i).id.name should equal ("t"+i)
    }
  }

  "resources" should "return all resources" in {
    val t1 = Task('t1, "Task one", 1, Some("Alice"))
    val t2 = Task('t2, "Task two", 1, Some("Bob"))
    val t3 = Task('t3, "Task three", 1, Some("Carol"))
    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set((t1 -> t2), (t2 -> t3))
    }
    p.resources should contain theSameElementsAs Seq("Alice", "Bob", "Carol")
  }

  it should "not repeat resources, even if they're in several tasks" in {
    val t1 = Task('t1, "Task one", 1, Some("Alice"))
    val t2 = Task('t2, "Task two", 1, Some("Alice"))
    val t3 = Task('t3, "Task three", 1, Some("Carol"))
    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set((t1 -> t2), (t2 -> t3))
    }
    p.resources should contain theSameElementsAs Seq("Alice", "Carol")
  }

  "schedule" should "produce a sensible schedule" in {
    val t1 = Task('t1, "Task one", 5, Some("Alice"))
    val t2 = Task('t2, "Task two", 3, Some("Bob"))
    val p = new Plan {
      val tasks = Set(t1, t2)
      val dependencies = Set((t1 -> t2))
    }
    val sch = p.schedule
    implicit val iSched = MatchingSchedule(sch)
    t1 should endRightBefore (t2)
  }
  
  "backingTasks" should "be empty if there is only one task" in {
    val t1 = Task('t1, 1.0)
    val p = new Plan {
      val tasks = Set(t1)
      val dependencies = Set[(Task,Task)]()
    }
    p.backingTasks(t1) should equal (Set())
  }
  
  it should "give the one predecessor if there is just one" in {
    val t1 = Task('t1, 1.0)
    val t2 = Task('t2, 1.0)
    val p = new Plan {
      val tasks = Set(t1, t2)
      val dependencies = Set((t1 -> t2))
    }
    p.backingTasks(t2) should equal (Set(t1))
  }
  
  it should "not give the only predecessor if it's not immediately before" in {
    val t1 = Task('t1, 1.0)
    val t2 = Task('t2, 1.0)
    val p = new Plan {
      val tasks = Set(t1, t2)
      val dependencies = Set((t1 -> t2))
      override lazy val schedule = new Schedule(Map((t1 -> 0), (t2 -> 10.0)))
    }
    p.backingTasks(t2) should equal (Set())
  }
  
  it should "give all predecessors that abutt the given one" in {
    val t1 = Task('t1, 1.0)
    val t2 = Task('t2, 1.0)
    val t3 = Task('t3, 1.0)
    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set((t1 -> t3), (t2 -> t3))
    }
    p.backingTasks(t3) should equal (Set(t1, t2))
  }
  
  it should "include a non-dependent task which abutts and uses the same resource" in {
    val t1 = Task('t1, "Task one", 1.0, Some("Alice"))
    val t2 = Task('t2, "Task two", 1.0, Some("Alice"))
    val t3 = Task('t3, "End task", 1.0, Some("Bob"))
    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set((t1 -> t3), (t2 -> t3))
      override lazy val schedule = new Schedule(Map((t1 -> 1.0), (t2 -> 0.0), (t3 -> 2.0)))
    }
    p.backingTasks(t1) should equal (Set(t2))
  }
  
  it should "exclude same-resource tasks which don't abutt" in {
    val t1 = Task('t1, "Task one", 1.0, Some("Alice"))
    val t2 = Task('t2, "Task two", 1.0, Some("Alice"))
    val t3 = Task('t3, "End task", 1.0, Some("Bob"))
    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set((t1 -> t3), (t2 -> t3))
      override lazy val schedule = new Schedule(Map((t1 -> 2.0), (t2 -> 0.0), (t3 -> 3.0)))
    }
    p.backingTasks(t1) should equal (Set())
  }

}