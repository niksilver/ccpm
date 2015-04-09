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

class PlanAdjusterTest extends FlatSpec with Matchers {

  "Move.samePiece" should "be false if the tasks are different" in {
    val m1 = Move(Task('t1), 2)
    val m2 = Move(Task('t2), 2)
    m1.samePiece(m2) should equal (false)
  }
  
  it should "be true for same task but different start" in {
    val m1 = Move(Task('t1), 2)
    val m2 = Move(Task('t1), 3)
    m1.samePiece(m2) should equal (true)
  }
  
  "Move.max" should "return this if other move is smaller" in {
    val m1 = Move(Task('t1), 5)
    val m2 = Move(Task('t1), 3)
    m1.max(m2) should equal (m1)
  }
  
  it should "return the other move if this move is smaller" in {
    val m1 = Move(Task('t1), 3)
    val m2 = Move(Task('t1), 5)
    m1.max(m2) should equal (m2)
  }
  
  "PlanAdjuster.attempt" should "return the full move if the task has one predecessor far back" in {
    val t1 = Task('t1, "Task one", 5, Some("Alice"))
    val t2 = Task('t2, "Task two", 3, Some("Bob"))
    val p = new Plan {
      val tasks = Set(t1, t2)
      val dependencies = Set((t1 -> t2))
      override lazy val schedule = new Schedule(Map(t1 -> 0, t2 -> 10.0))
      override lazy val criticalChain = Seq()
    }
    
    val adjuster = new PlanAdjuster
    val att = adjuster.attempt(p, Move(t2, 7.0))
    att should equal (Seq(Actual(Move(t2, 7.0))))
  }
  
  it should "return a prerequisite if the task has one predecessor slightly behind (1)" in {
    val t1 = Task('t1, "Task one", 5, Some("Alice"))
    val t2 = Task('t2, "Task two", 3, Some("Bob"))
    val p = new Plan {
      val tasks = Set(t1, t2)
      val dependencies = Set((t1 -> t2))
      override lazy val schedule = new Schedule(Map(t1 -> 0, t2 -> 6.0))
      override lazy val criticalChain = Seq()
    }
    
    val adjuster = new PlanAdjuster
    val att = adjuster.attempt(p, Move(t2, 4.0))
    att should equal (Seq(Prerequisite(Move(t1, -1.0))))
  }
  
  it should "return a prerequisite if the task has one predecessor slightly behind (2 - to avoid faking)" in {
    val t1 = Task('t1, "Task one", 4, Some("Alice"))
    val t2 = Task('t2, "Task two", 4, Some("Bob"))
    val p = new Plan {
      val tasks = Set(t1, t2)
      val dependencies = Set((t1 -> t2))
      override lazy val schedule = new Schedule(Map(t1 -> 3, t2 -> 10))
      override lazy val criticalChain = Seq()
    }
    
    val adjuster = new PlanAdjuster
    val att = adjuster.attempt(p, Move(t2, 5))
    att should equal (Seq(Prerequisite(Move(t1, 1))))
  }
  
  it should "return two prerequisites if the task has two joint predecessors different distances behind" in {
    val t1a = Task('t1a, "Task 1a", 4, Some("Alice"))
    val t1b = Task('t1b, "Task 1b", 4, Some("Bob"))
    val t2 = Task('t2, "Task two", 4, Some("Bob"))
    val p = new Plan {
      val tasks = Set(t1a, t1b, t2)
      val dependencies = Set(t1a -> t2, t1b -> t2)
      override lazy val schedule = new Schedule(Map(t1a -> 1, t1b -> 2, t2 -> 6))
      override lazy val criticalChain = Seq()
    }
    
    val adjuster = new PlanAdjuster
    val att = adjuster.attempt(p, Move(t2, 4))
    att should equal (Seq(Prerequisite(Move(t1a, 0)), Prerequisite(Move(t1b, 0))))
  }
  
  it should "not attempt to move a task which is on the critical chain" in {
    //       /---[t2 2]-[t3 2]\
    // [t1 1]+[t4 5          ]+[t5 1]
    val t1 = Task('t1, "Task one", 1, None)
    val t2 = Task('t2, "Task two", 2, None)
    val t3 = Task('t3, "Task three", 2, None)
    val t4 = Task('t4, "Task four", 5, None)
    val t5 = Task('t5, "Task five", 1, None)
    val p = new Plan {
      val tasks = Set(t1, t2, t3, t4, t5)
      val dependencies = Set(t1 -> t2, t2 -> t3, t3 -> t5,
          t1 -> t4, t4 -> t5)
    }
    
    // Check the critical chain is as we think
    p.criticalChain should equal (Seq(t1, t4, t5))
    
    // Attempting to move back t2 should not allow a move of t1
    val t2Start = p.schedule.start(t2)
    val adjuster = new PlanAdjuster
    val att = adjuster.attempt(p, Move(t2, t2Start - 2))
    att should equal (Seq())
  }
  
  "PlanAdjuster.move" should "move a task back to its predecessor if asked and it's possible (1)" in {
    val t1 = Task('t1, "Task one", 5, Some("Alice"))
    val t2 = Task('t2, "Task two", 3, Some("Bob"))
    val p = new Plan {
      val tasks = Set(t1, t2)
      val dependencies = Set(t1 -> t2)
      override lazy val schedule = new Schedule(Map(t1 -> 0, t2 -> 10.0))
      override lazy val criticalChain = Seq()
    }
    
    val adjuster = new PlanAdjuster
    val (p2, actualMove) = adjuster.move(p, Move(t2, 5))
    p2.schedule.start(t2) should equal (p2.schedule.end(t1))
    actualMove should equal (Move(t2, 5))
  }
  
  it should "move a task back to its predecessor if asked and it's possible (2 - to avoid faking)" in {
    val t1 = Task('t1, "Task one", 6, Some("Alice"))
    val t2 = Task('t2, "Task two", 3, Some("Bob"))
    val p = new Plan {
      val tasks = Set(t1, t2)
      val dependencies = Set(t1 -> t2)
      override lazy val schedule = new Schedule(Map(t1 -> 0, t2 -> 10.0))
      override lazy val criticalChain = Seq()
    }
    
    val adjuster = new PlanAdjuster
    val (p2, actualMove) = adjuster.move(p, Move(t2, 6))
    p2.schedule.start(t2) should equal (p2.schedule.end(t1))
    actualMove should equal (Move(t2, 6))
  }
  
  it should "move a task back to its predecessor if it can't go as far as we'd like" in {
    val t1 = Task('t1, "Task one", 6, Some("Alice"))
    val t2 = Task('t2, "Task two", 3, Some("Bob"))
    val p = new Plan {
      val tasks = Set(t1, t2)
      val dependencies = Set(t1 -> t2)
      override lazy val schedule = new Schedule(Map(t1 -> 0, t2 -> 10.0))
      override lazy val criticalChain = Seq()
    }
    
    val adjuster = new PlanAdjuster
    val (p2, actualMove) = adjuster.move(p, Move(t2, 2))
    p2.schedule.start(t2) should equal (p2.schedule.end(t1))
    actualMove should equal (Move(t2, 6))
  }
  
  it should "move a task back to a resource-conflicting task if that's only as far as it can go" in {

    //   [t1 A]
    // [t2 B]----[t3 A]

    val t1 = Task('t1, "Task one", 4, Some("Alice"))
    val t2 = Task('t2, "Task two", 4, Some("Bob"))
    val t3 = Task('t3, "Task three", 4, Some("Alice"))
    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t2 -> t3)
      override lazy val schedule = new Schedule(Map(t1 -> 2, t2 -> 0, t3 -> 8))
      override lazy val criticalChain = Seq()
    }
    
    val adjuster = new PlanAdjuster
    val (p2, actualMove) = adjuster.move(p, Move(t3, 4))
    p2.schedule.start(t3) should equal (p2.schedule.end(t1))
    actualMove should equal (Move(t3, 6))
  }
  
  it should "not move a task if it's already backed up to its predecessor" in {

    // [t1 B][t2 A]

    val t1 = Task('t1, "Task one", 4, Some("Bob"))
    val t2 = Task('t2, "Task two", 4, Some("Alice"))
    val p = new Plan {
      val tasks = Set(t1, t2)
      val dependencies = Set(t1 -> t2)
      override lazy val schedule = new Schedule(Map(t1 -> 0, t2 -> 4))
      override lazy val criticalChain = Seq()
    }
    
    val adjuster = new PlanAdjuster
    val (p2, actualMove) = adjuster.move(p, Move(t2, -4))
    p2.schedule.start(t2) should equal (p2.schedule.end(t1))
    actualMove should equal (Move(t2, 4))
  }
  
  it should "not move a task if it's on the critical chain" in {

    // [t1 B]---[t2 A]

    val t1 = Task('t1, "Task one", 4, Some("Bob"))
    val t2 = Task('t2, "Task two", 4, Some("Alice"))
    val p = new Plan {
      val tasks = Set(t1, t2)
      val dependencies = Set(t1 -> t2)
      override lazy val schedule = new Schedule(Map(t1 -> 0, t2 -> 5))
    }
    
    // Check the critical chain is as we expect
    p.criticalChain should equal (Seq(t1, t2))
    
    val adjuster = new PlanAdjuster
    val (p2, actualMove) = adjuster.move(p, Move(t2, 4))
    p2.schedule.start(t2) should equal (5)
    actualMove should equal (Move(t2, 5))
    
  }
}
