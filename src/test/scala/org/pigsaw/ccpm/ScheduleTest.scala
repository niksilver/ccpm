package org.pigsaw.ccpm

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

class ScheduleTest extends FlatSpec with Matchers {

  // The following definitions allow us to more easily assert
  // that a half-duration task should come right before another task:
  //
  //   t1 should halfBeRightBefore t2
  //
  // See http://www.scalatest.org/user_guide/using_matchers#usingCustomMatchers
  
  case class MatchingSchedule(sch: Schedule)
  
  class TaskHalfBeRightBefore(tLater: Task, sch: Schedule) extends Matcher[Task] {
    def apply(tEarlier: Task) = {
      val halfEnd = sch.halfEnd(tEarlier)
      val earlierStart = sch.start(tEarlier)
      val laterStart = sch.start(tLater)
      MatchResult(
           halfEnd == laterStart,
          s"$tEarlier with start $earlierStart did not come right before $tLater with start $laterStart",
          s"$tEarlier with start $earlierStart came right before $tLater with start $laterStart"
      )
    }
  }
  
  def halfBeRightBefore(tEarlier: Task)(implicit iSched: MatchingSchedule) = new TaskHalfBeRightBefore(tEarlier, iSched.sch)

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

  "Schedule.start" should "get the start times of several added tasks" in {
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

  "Schedule.end" should "get the end times of several added tasks" in {
    val t0 = new Task('t0, "My task", 2, Some("Alice"))
    val t1 = new Task('t1, "Task 2", 3, Some("Bob"))
    val sch = new Schedule() + (t0, 5) + (t1, 6)
    sch.end(t0) should equal (5 + t0.duration)
    sch.end(t1) should equal (6 + t1.duration)
  }

  it should "throw an UnknownTaskException if we try to get the end time of an unknown task" in {
    an[UnknownTaskException] should be thrownBy {
      val t = new Task('t0, "My task", 5, Some("Alice"))
      val sch = new Schedule()
      sch.end(t)
    }
  }

  "Schedule.halfEnd" should "get the end times of several added half-tasks" in {
    val t0 = new Task('t0, "My task", 2, Some("Alice"))
    val t1 = new Task('t1, "Task 2", 3, Some("Bob"))
    val sch = new Schedule() + (t0, 5) + (t1, 6)
    sch.halfEnd(t0) should equal (5 + t0.duration/2)
    sch.halfEnd(t1) should equal (6 + t1.duration/2)
  }

  it should "throw an UnknownTaskException if we try to get the half-end time of an unknown task" in {
    an[UnknownTaskException] should be thrownBy {
      val t = new Task('t0, "My task", 5, Some("Alice"))
      val sch = new Schedule()
      sch.end(t)
    }
  }
  
  "Schedule.resourceConflicts" should "be false if no tasks scheduled" in {
    val sch = new Schedule()
    val t = new Task('t, "My task", 5, Some("Alice"))
    sch.resourceConflicts(t, 4.0) should be (false)
  }
  
  it should "be true if there's one task scheduled and there's a clear resource conflict at the end" in {
    val t99 = Task('t99, "My 99", 5, Some("Alice"))
    val t01 = Task('t01, "My first", 5, Some("Alice"))
    val sch = new Schedule() + (t99, 20)
    // Remember we're working with half-durations
    sch.resourceConflicts(t01, 18) should be (true)
  }
  
  it should "be true if there's one task scheduled and there's a clear resource conflict at the start" in {
    val t99 = Task('t99, "My 99", 5, Some("Alice"))
    val t01 = Task('t01, "My first", 5, Some("Alice"))
    val sch = new Schedule() + (t99, 20)
    // Remember we're working with half-durations
    sch.resourceConflicts(t01, 21) should be (true)
  }
  
  it should "be true if there's one longer task scheduled (same resource) with the same start" in {
    val t99 = Task('t99, "My 99", 5, Some("Alice"))
    val t01 = Task('t01, "My first", 1, Some("Alice"))
    val sch = new Schedule() + (t99, 20)
    // Remember we're working with half-durations
    sch.resourceConflicts(t01, 20) should be (true)
  }
  
  it should "be true if there's one shorter task scheduled (same resource) with the same start" in {
    val t99 = Task('t99, "My 99", 1, Some("Alice"))
    val t01 = Task('t01, "My first", 5, Some("Alice"))
    val sch = new Schedule() + (t99, 20)
    // Remember we're working with half-durations
    sch.resourceConflicts(t01, 20) should be (true)
  }
  
  it should "be true if there's one longer task scheduled (same resource) with the same end" in {
    val t99 = Task('t99, "My 99", 5, Some("Alice"))
    val t01 = Task('t01, "My first", 1, Some("Alice"))
    val sch = new Schedule() + (t99, 20)
    // Remember we're working with half-durations
    sch.resourceConflicts(t01, 22) should be (true)
  }
  
  it should "be true if there's one shorter task scheduled (same resource) with the same end" in {
    val t99 = Task('t99, "My 99", 1, Some("Alice"))
    val t01 = Task('t01, "My first", 5, Some("Alice"))
    val sch = new Schedule() + (t99, 20)
    // Remember we're working with half-durations
    sch.resourceConflicts(t01, 18) should be (true)
  }
  
  it should "be false if the new task is zero duration but otherwise conflicts at the start" in {
    val t99 = Task('t99, "My 99", 5, Some("Alice"))
    val t01 = Task('t01, "My first", 0, Some("Alice"))
    val sch = new Schedule() + (t99, 20)
    // Remember we're working with half-durations
    sch.resourceConflicts(t01, 20) should be (false)
  }
  
  it should "be false if the new task is zero duration but otherwise conflicts at the end" in {
    val t99 = Task('t99, "My 99", 5, Some("Alice"))
    val t01 = Task('t01, "My first", 0, Some("Alice"))
    val sch = new Schedule() + (t99, 20)
    // Remember we're working with half-durations
    sch.resourceConflicts(t01, 22.5) should be (false)
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
    
    // Note critical chain requires scheduling to half task duration
    sch.halfEnd(tAlice1) should equal (sch.start(tAlice2))
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

    // Note critical chain requires scheduling to half task duration
    sch2.halfEnd(tAlice1) should equal (sch2.start(tAlice2))
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
    
    // Note critical chain requires scheduling to half task duration
    implicit val iSched = new MatchingSchedule(sch2)
    tAlice3 should halfBeRightBefore (tAlice4)
    tAlice2 should halfBeRightBefore (tAlice3)
    tAlice1 should halfBeRightBefore (tAlice2)
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
    sch2.halfEnd(t1) should equal (sch2.start(t3))
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
    sch2.halfEnd(t1) should equal (sch2.start(t4))
    
    implicit val iSched = new MatchingSchedule(sch2)
    t1 should halfBeRightBefore (t4)
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
    sch2.halfEnd(t1) should equal (sch2.start(t4))
  }
  
  ignore should "schedule many tasks in the right order according to dependencies and resources" in {
    val tStart = Task('start)
    val a1 = new Task('a1, "a1", 2*3, Some("C"))
    val a2 = new Task('a2, "a2", 2*4, Some("D"))
    val a3 = new Task('a3, "a3", 2*1.5, Some("E"))
    val b1 = new Task('b1, "b1", 2*5, Some("A"))
    val b2 = new Task('b2, "b2", 2*5, Some("B"))
    val c1 = new Task('c1, "c1", 2*3, Some("B"))
    val c2 = new Task('c2, "c2", 2*4, Some("B"))
    val c3 = new Task('c3, "c3", 2*5, Some("A"))
    val c4 = new Task('c4, "c4", 2*2.5, Some("C"))
    val tEnd = Task('end)
    
    val deps = List(
        tStart -> a1, tStart -> a2, tStart -> a3,
        tStart -> c1, tStart -> c2,
        a1 -> b1, a2 -> b1, a3 -> b1,
        b1 -> b2,
        c1 -> c3, c2 -> c3,
        c3 -> c4,
        b2 -> tEnd, c4 -> tEnd
    )
    
    val tasks = List(tStart, a1, a2, a3, b1, b2, c1, c2, c3, c4, tEnd)
    
    val sch = (new Schedule()).schedule(tasks, deps)
    implicit val iSched = new MatchingSchedule(sch)
    
    // Here's our intended schedule:
    // [id, half-duration, resource]
    // 
    // [start,0]
    //    +---[a1, 3,     C]-+
    //    +-[a2, 4,       D]-+
    //    +-----[a3, 1.5, E]-+
    //    |                  \[b1, 5,   A]--------\
    //    |                                       +[b2, 5,        B]\
    //    +---------------------[c1, 3, B]\                         |
    //    |                               +[c3, 5,   A]\            |
    //    |                               |            +[c4, 2.5, C]\
    //    \----------[c2, 4,  B]----------/                         |
    //                                                              +[end,0]
    
    // The sequence b1, b2, tEnd
    
    b2 should halfBeRightBefore (tEnd)
    c4 should halfBeRightBefore (tEnd)
    
    // The sequence tStart, c1, c2, c3, c4, tEnd, including resource conflict c1, c2
    
    c3 should halfBeRightBefore (c4)
    val laterOfC1AndC2 = if (sch.halfEnd(c1) > sch.halfEnd(c2)) c1 else c2
    val earlierOfC1AndC2 = if (sch.start(c1) < sch.start(c2)) c1 else c2
    laterOfC1AndC2 should halfBeRightBefore (c3)
    earlierOfC1AndC2 should halfBeRightBefore (laterOfC1AndC2)
    sch.halfEnd(tStart) should be < sch.start(c1)
    sch.halfEnd(tStart) should be < sch.start(c2)
    
    // Resource conflict b1, c3
    
    b1 should halfBeRightBefore (c3)
    
    // The sequence tStart, a1, a2, a3, b1
    
    a1 should halfBeRightBefore (b1)
    a2 should halfBeRightBefore (b1)
    a3 should halfBeRightBefore (b1)
    sch.halfEnd(tStart) should be < sch.start(a1)
    a2 should halfBeRightBefore (b1)
    sch.halfEnd(tStart) should be < sch.start(a3)
  }

}
