package org.pigsaw.ccpm

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class PlanTest extends FlatSpec with Matchers with ScheduleMatchers {

  "A Plan" should "be constructable with an empty block" in {
    new Plan {}
  }

  it should "be able to accept a Task" in {
    val p = new Plan {
      Task("My task 1")
    }
  }

  "tasks" should "be able to return the list of tasks specified (1)" in {
    val p1 = new Plan {
      add task "My task"
    }
    println("Starting assertions")
    (p1.tasks)(0) should equal(Task("My task"))
    p1.tasks.length should equal(1)
  }

  it should "be able to return the list of tasks specified (2 - to avoid faking)" in {
    val p2 = new Plan {
      add task "My task 1"
      add task "My task 2"
    }
    (p2.tasks)(0).description should equal("My task 1")
    (p2.tasks)(1).description should equal("My task 2")
    p2.tasks.length should equal(2)
  }
  
  "schedule" should "produce a sensible schedule" in {
    val p = new Plan {
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

}
