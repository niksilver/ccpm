package org.pigsaw.ccpm

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class ScheduleTest extends FlatSpec with Matchers {
  
  "Schedule" should "allow the addition of a new task and its start time (1)" in {
    val t = new Task('t0, "My task", 5, Some("Alice"))
    val sch = new Schedule()
    val sch2 = sch + (t, 10)
    sch2.start(t) should equal (10)
  }
  
  it should "allow the addition of a new task and its start time (2 - to avoid faking it)" in {
    val t = new Task('t0, "My task", 5, Some("Alice"))
    val sch = new Schedule()
    val sch2 = sch + (t, 20)
    sch2.start(t) should equal (20)
  }
  
  it should "allow the addition of several new tasks and their start times" in {
    val t0 = new Task('t0, "My task", 2, Some("Alice"))
    val t1 = new Task('t1, "Task 2",  3, Some("Bob"))
    val sch = new Schedule()
    val sch2 = sch + (t0, 5) + (t1, 6)
    sch2.start(t0) should equal (5)
    sch2.start(t1) should equal (6)
  }
  
  it should "schedule the first task at some arbitrary time" in {
    val sch1 = new Schedule()
    val t = new Task('t0, "My task", 5, Some("Alice"))
    val sch2 = sch1.schedule(t)
    sch2.start(t) shouldBe an [Int]
  }
}
