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
  
  it should "allow the addition of a new task at a non-integer time" in {
    val t = new Task('t0, "My task", 5, Some("Alice"))
    val sch = new Schedule()
    val sch2 = sch + (t, 1.5)
    sch2.start(t) should equal (1.5)
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
    
    // Note critical chain requires scheduling to half task duration
    (start1 + tAlice1.halfDuration) should equal (start2)
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

    // Note critical chain requires scheduling to half task duration
    (start1 + tAlice1.halfDuration) should equal (start2)
  }
  
  it should "schedule a resource-conflicting task to be just before the resource is available"+
  		" even if the conflicting task is among others and there are several resource-conflicts" in {
    val tAlice1 = new Task('a1, "First task", 2, Some("Alice"))
    val tAlice2 = new Task('a2, "Second task", 3, Some("Alice"))
    val tAlice3 = new Task('a3, "Third task", 4, Some("Alice"))
    val tAlice4 = new Task('a4, "Forth task", 5, Some("Alice"))
    val tOther1 = new Task('o1, "Other one", 6, Some("Bob"))
    val tOther2 = new Task('o2, "Other two", 7, Some("Carla"))

    // First we schedule the last tasks
    val sch1 = (new Schedule()).schedule(tOther1).
    	schedule(tAlice4).schedule(tAlice3).schedule(tAlice2).
    	schedule(tOther2)
    // Then we schedule the resource-conflicting task
    val sch2 = sch1.schedule(tAlice1)
    
    val start1 = sch2.start(tAlice1)
    val start2 = sch2.start(tAlice2)
    val start3 = sch2.start(tAlice3)
    val start4 = sch2.start(tAlice4)

    // Note critical chain requires scheduling to half task duration

    (start3 + tAlice3.halfDuration) should equal (start4)
    (start2 + tAlice2.halfDuration) should equal (start3)
    (start1 + tAlice1.halfDuration) should equal (start2)
  }

  it should "schedule a task before a given other" in {
    val t1 = new Task('t1, "Task one", 1, Some("Alice"))
    val t2 = new Task('t2, "Task two", 2, Some("Bob"))
    val t3 = new Task('t3, "Task three", 3, Some("Carol"))
    val t4 = new Task('t4, "Task four", 4, Some("Dan"))
    val t5 = new Task('t5, "Task five", 5, Some("Eve"))
    
    // We'll schedule all but t1
    val sch1 = (new Schedule()).schedule(t5).schedule(t4).schedule(t3).schedule(t2)
    
    // Now schedule t1 to be before t3
    val sch2 = sch1.schedule(t1, List(t3))
    
    // Note critical chain requires scheduling to half task duration
    (sch2.start(t1) + t1.halfDuration) should equal (sch2.start(t3))
  }

  it should "schedule a task before several given others" in {
    val t1 = new Task('t1, "Task one", 1, Some("Alice"))
    val t2 = new Task('t2, "Task two", 2, Some("Bob"))
    val t3 = new Task('t3, "Task three", 3, Some("Carol"))
    val t4 = new Task('t4, "Task four", 4, Some("Dan"))
    val t5 = new Task('t5, "Task five", 5, Some("Eve"))
    
    // We'll schedule t2, t3, and t4 all before t5
    // but won't yet schedule t1
    val sch1 = (new Schedule()).schedule(t5).
    	schedule(t4, List(t5)).
    	schedule(t3, List(t5)).
    	schedule(t2, List(t5))
    
    // Now schedule t1 to be before t4, t3 and t2
    // We're careful to put the earliest task, t4, in the middle
    val sch2 = sch1.schedule(t1, List(t3, t4, t2))
    
    // t1 should start and finish just before the earliest task: t4
    // Remember, critical chain requires we schedule by half-duration
    (sch2.start(t1) + t1.halfDuration) should equal (sch2.start(t4))
  }

  it should "schedule a task before several given others or any with a resource conflict" in {
    val t1 = new Task('t1, "Task one", 1, Some("Alice"))
    val t2 = new Task('t2, "Task two", 2, Some("Bob"))
    val t3 = new Task('t3, "Task three", 3, Some("Carol"))
    val t4 = new Task('t4, "Task four", 4, Some("Alice")) // Resource conflict with t1
    val t5 = new Task('t5, "Task five", 5, Some("Eve"))
    
    // We'll schedule t2, t3, and t4 all before t5
    // but won't yet schedule t1
    val sch1 = (new Schedule()).schedule(t5).
    	schedule(t4, List(t5)).
    	schedule(t3, List(t5)).
    	schedule(t2, List(t5))
    
    // Now schedule t1 to be before t3 and t2.
    // We don't list t4; it should work out itself that it needs
    // to avoid this resource-conflicting task
    val sch2 = sch1.schedule(t1, List(t3, t2))
    
    // t1 should start and finish just before the earliest task: t4
    // Remember, critical chain requires we schedule by half-duration
    (sch2.start(t1) + t1.halfDuration) should equal (sch2.start(t4))
  }

}
