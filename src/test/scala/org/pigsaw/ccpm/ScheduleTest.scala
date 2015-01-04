package org.pigsaw.ccpm

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class ScheduleTest extends FlatSpec with Matchers {

  "Schedule.add" should "allow the addition of a new task and its start time (1)" in {
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

  "Schedule.start" should "allow the addition of several new tasks and their start times" in {
    val t0 = new Task('t0, "My task", 2, Some("Alice"))
    val t1 = new Task('t1, "Task 2", 3, Some("Bob"))
    val sch = new Schedule()
    val sch2 = sch + (t0, 5) + (t1, 6)
    sch2.start(t0) should equal (5)
    sch2.start(t1) should equal (6)
  }

  it should "throw an UnknownTaskException if we try to get the start time of an unknown task" in {
    an[UnknownTaskException] should be thrownBy {
      val t = new Task('t0, "My task", 5, Some("Alice"))
      val sch = new Schedule()
      sch.start(t)
    }
  }

  "Schedule.schedule" should "schedule the first task at some arbitrary time" in {
    val sch0 = new Schedule()
    val t = new Task('t0, "My task", 5, Some("Alice"))
    val sch1 = sch0.schedule(t)
    noException should be thrownBy {
      sch1.start(t)
    }
  }

  it should "be able to schedule more than one task" in {
    val sch0 = new Schedule()
    val t0 = new Task('t0, "My task", 5, Some("Alice"))
    val t1 = new Task('t1, "Task two", 5, Some("Bob"))
    val sch1 = sch0.schedule(t0).schedule(t1)
    noException should be thrownBy {
      sch1.start(t0)
      sch1.start(t1)
    }
  }

  it should "schedule all non-conflicting end tasks at the same time" in {
    val t0 = new Task('t0, "My task", 2, Some("Alice"))
    val t1 = new Task('t1, "Task 2", 3, Some("Bob"))
    val sch0 = new Schedule()
    val sch1 = sch0.schedule(t0).schedule(t1)
    sch1.start(t0) should equal (sch1.start(t1))
  }
  
  it should "schedule a resource-conflicting task to be just before the resource is available" in {
    val tAlice1 = new Task('a1, "First task", 2, Some("Alice"))
    val tAlice2 = new Task('a2, "Second task", 3, Some("Alice"))

    // Notice we schedule the last task first...
    val sch = (new Schedule()).schedule(tAlice2).schedule(tAlice1)
    val start1 = sch.start(tAlice1)
    val start2 = sch.start(tAlice2)
    (start1 + tAlice1.duration) should equal (start2)
  }
  
  it should "schedule a resource-conflicting task to be just before the resource is available"+
  		" even if the conflicting task is among others" in {
    val tAlice1 = new Task('a1, "First task", 2, Some("Alice"))
    val tAlice2 = new Task('a2, "Second task", 3, Some("Alice"))
    val tOther1 = new Task('o1, "Other one", 4, Some("Bob"))
    val tOther2 = new Task('o2, "Other two", 4, Some("Carla"))

    // First we schedule the last tasks
    val sch1 = (new Schedule()).schedule(tOther1).schedule(tAlice2).schedule(tOther2)
    // Then we schedule the resource-conflicting task
    val sch2 = sch1.schedule(tAlice1)
    
    val start1 = sch2.start(tAlice1)
    val start2 = sch2.start(tAlice2)
    (start1 + tAlice1.duration) should equal (start2)
  }
}
