package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class PlanTest extends FlatSpec with Matchers with ScheduleMatchers {
  
  "resources" should "return all resources" in {
    val t1 = Task('t1, "Task one", 1, Some("Alice"))
    val t2 = Task('t2, "Task two", 1, Some("Bob"))
    val t3 = Task('t3, "Task three", 1, Some("Carol"))
    val p = new Plan {
      val tasks = Seq(t1, t2, t3)
      val dependencies = Seq((t1 -> t2), (t2 -> t3))
    }
    p.resources should contain theSameElementsAs Seq("Alice", "Bob", "Carol")
  }
  
  it should "not repeat resources, even if they're in several tasks" in {
    val t1 = Task('t1, "Task one", 1, Some("Alice"))
    val t2 = Task('t2, "Task two", 1, Some("Alice"))
    val t3 = Task('t3, "Task three", 1, Some("Carol"))
    val p = new Plan {
      val tasks = Seq(t1, t2, t3)
      val dependencies = Seq((t1 -> t2), (t2 -> t3))
    }
    p.resources should contain theSameElementsAs Seq("Alice", "Carol")
  }
  
  "schedule" should "produce a sensible schedule" in {
    val p = new ScriptedPlan {
      declare resource "Alice"
      declare resource "Bob"
      add task 't1 as "Task one" duration 5 resource "Alice"
      add task 't2 as "Task two" duration 3 resource "Bob"
      't1 ~> 't2
    }
    val sch = p.schedule
    implicit val iSched = MatchingSchedule(sch)
    p.task('t1) should halfEndRightBefore (p.task('t2))
  }
  
  "chains" should "return paths if no resource conflicts" in {
    val p = new ScriptedPlan {
      add task 'a1 duration 1
      add task 'a2 duration 1
      add task 'b  duration 1
      add task 'c  duration 1
      add task 'd1 duration 1
      add task 'd2 duration 1
      'a1 ~> 'b ~> 'c ~> 'd1
      'a2 ~> 'b
      'c ~> 'd2
    }
    val a1 = p.task('a1)
    val a2 = p.task('a2)
    val b  = p.task('b)
    val c  = p.task('c)
    val d1 = p.task('d1)
    val d2 = p.task('d2)
    val chains = p.chains
    chains should contain (Seq(a1, b, c, d1))
    chains should contain (Seq(a1, b, c, d2))
    chains should contain (Seq(a2, b, c, d1))
    chains should contain (Seq(a2, b, c, d2))
    chains.length should equal (4)
  }

}