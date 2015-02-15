package org.pigsaw.ccpm

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
      override lazy val schedule = new Schedule(Map((t1 -> 0), (t2 -> 10.0)))
    }
    val sch = p.schedule
    
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
      override lazy val schedule = new Schedule(Map((t1 -> 0), (t2 -> 6.0)))
    }
    val sch = p.schedule
    
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
      override lazy val schedule = new Schedule(Map((t1 -> 3), (t2 -> 10)))
    }
    val sch = p.schedule
    
    val adjuster = new PlanAdjuster
    val att = adjuster.attempt(p, Move(t2, 5))
    att should equal (Seq(Prerequisite(Move(t1, 1))))
  }
}