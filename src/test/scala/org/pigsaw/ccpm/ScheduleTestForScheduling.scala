package org.pigsaw.ccpm

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class ScheduleTestForScheduling extends FlatSpec with Matchers with ScheduleMatchers {

  "schedule(Task)" should "schedule the first task at some arbitrary time" in {
    val sch0 = new Schedule()
    val t = new Task('t0, "My task", 5, Some("Alice"))
    val sch1 = sch0.schedule(t)
    sch1.start(t) should equal (Schedule.defaultStart)
  }

  it should "schedule two non-conflicting tasks to end at the same time" in {
    val t0 = new Task('t0, "My task", 2, Some("Alice"))
    val t1 = new Task('t1, "Task 2", 3, Some("Bob"))
    val sch0 = new Schedule()
    val sch1 = sch0.schedule(t0).schedule(t1)
    sch1.end(t0) should equal (sch1.end(t1))
  }

  it should "schedule a non-conflicting tasks to end at the latest time, even if there's an earlier task" in {
    val t0 = new Task('t0, "My task", 2, Some("Alice"))
    val t1 = new Task('t1, "Task 2", 3, Some("Bob"))
    val t2 = new Task('t2, "Task 3", 4, Some("Carol"))
    val sch1 = new Schedule() +
    	(t0, 20) +    // Ends at 21
    	(t1, 25)      // Ends at 26.5
    val sch2 = sch1.schedule(t2)
    sch2.end(t2) should equal (sch2.end(t1))
  }

  it should "schedule a resource-conflicting task to be just before the resource is available "+
  		"(1 - first task has default start)" in {
    val tAlice1 = new Task('a1, "First task", 2, Some("Alice"))
    val tAlice2 = new Task('a2, "Second task", 3, Some("Alice"))

    // Notice we schedule the last task first...
    val sch = (new Schedule()).schedule(tAlice2).schedule(tAlice1)

    sch.end(tAlice1) should equal (sch.start(tAlice2))
  }

  it should "schedule a resource-conflicting task to be just before the resource is available "+
  		"(2 - first task later than default start)" in {
    val tAlice1 = new Task('a1, "First task", 2, Some("Alice"))
    val tAlice2 = new Task('a2, "Second task", 3, Some("Alice"))

    val tAlice2Start = Schedule.defaultStart + 20
    // Notice we schedule the last task first...
    val sch = (new Schedule() + (tAlice2, tAlice2Start)).schedule(tAlice1)

    sch.end(tAlice1) should equal (sch.start(tAlice2))
  }

  it should "schedule a resource-conflicting task to be just before the resource is available "+
  		"(3 - first task earlier than default start)" in {
    val tAlice1 = new Task('a1, "First task", 2, Some("Alice"))
    val tAlice2 = new Task('a2, "Second task", 3, Some("Alice"))

    val tAlice2Start = Schedule.defaultStart - 20
    // Notice we schedule the last task first...
    val sch = (new Schedule() + (tAlice2, tAlice2Start)).schedule(tAlice1)

    sch.end(tAlice1) should equal (sch.start(tAlice2))
  }

  it should "schedule a resource-conflicting task to be just before the resource is available" +
    " even if the conflicting task is among others" in {
      val tAlice1 = new Task('a1, "First task", 2, Some("Alice"))
      val tAlice2 = new Task('a2, "Second task", 3, Some("Alice"))
      val tOther1 = new Task('o1, "Other one", 4, Some("Bob"))
      val tOther2 = new Task('o2, "Other two", 4, Some("Carla"))

      // First we schedule the last tasks
      val sch1 = (new Schedule()).schedule(tOther1).schedule(tAlice2).schedule(tOther2)
      // Then we schedule the resource-conflicting task
      val sch2 = sch1.schedule(tAlice1)

      sch2.end(tAlice1) should equal (sch2.start(tAlice2))
    }

  it should "schedule a resource-conflicting task between others if the time is available" in {
    val tAlice1 = new Task('a1, "First task", 1, Some("Alice"))
    val tAlice2 = new Task('a2, "Second task", 1.5, Some("Alice"))
    val tAlice3 = new Task('a3, "Middle task", 1.5, Some("Alice"))

    // First we schedule two tasks with a gap
    val sch1 = (new Schedule()) +
      (tAlice1, 10) + // Runs 10 - 11
      (tAlice2, 6) // Runs 6 - 7.5

    // Then we schedule one which should go in between
    val sch2 = sch1.schedule(tAlice3)

    sch2.end(tAlice3) should equal (sch2.start(tAlice1))
  }

  it should "schedule a resource-conflicting task after another if the time is available" in {
    val t1 = new Task('t1, "First task", 1, Some("Alice"))
    val t2 = new Task('t2, "Second task", 1.5, Some("Alice"))
    val t3 = new Task('t3, "Third task", 1.5, Some("Bob"))

    // First we schedule two tasks with a gap
    val sch1 = (new Schedule()) +
      (t3, 10) + // Runs 10 - 11.5
      (t2, 8.5)  // Runs 8.5 - 10

    // Then we schedule one which should go right at the end
    val sch2 = sch1.schedule(t1)

    sch2.end(t1) should equal (sch2.end(t3))
  }

  it should "schedule a resource-conflicting task to be just before the resource is available" +
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

      implicit val iSched = new MatchingSchedule(sch2)
      tAlice3 should endRightBefore (tAlice4)
      tAlice2 should endRightBefore (tAlice3)
      tAlice1 should endRightBefore (tAlice2)
    }

  "scheduleBefore" should "schedule a task before a given other" in {
    val t1 = new Task('t1, "Task one", 1, Some("Alice"))
    val t2 = new Task('t2, "Task two", 2, Some("Bob"))
    val t3 = new Task('t3, "Task three", 3, Some("Carol"))
    val t4 = new Task('t4, "Task four", 4, Some("Dan"))
    val t5 = new Task('t5, "Task five", 5, Some("Eve"))

    // We'll schedule all but t1
    val sch1 = (new Schedule()).schedule(t5).schedule(t4).schedule(t3).schedule(t2)

    // Now schedule t1 to be before t3
    val sch2 = sch1.scheduleBefore(t1, List(t3))

    sch2.end(t1) should equal (sch2.start(t3))
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
      scheduleBefore(t4, List(t5)).
      scheduleBefore(t3, List(t5)).
      scheduleBefore(t2, List(t5))

    // Now schedule t1 to be before t4, t3 and t2
    // We're careful to put the earliest task, t4, in the middle
    val sch2 = sch1.scheduleBefore(t1, List(t3, t4, t2))

    // t1 should start and finish just before the earliest task: t4
    sch2.end(t1) should equal (sch2.start(t4))

    implicit val iSched = new MatchingSchedule(sch2)
    t1 should endRightBefore (t4)
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
      scheduleBefore(t4, List(t5)).
      scheduleBefore(t3, List(t5)).
      scheduleBefore(t2, List(t5))

    // Now schedule t1 to be before t3 and t2.
    // We don't list t4; it should work out itself that it needs
    // to avoid this resource-conflicting task
    val sch2 = sch1.scheduleBefore(t1, List(t3, t2))

    // t1 should start and finish just before the earliest task: t4
    sch2.end(t1) should equal (sch2.start(t4))
  }

  "Schedule.make" should "schedule the latest-ending tasks first "+
  		"(1 - latest-ending task is in middle of others)" in {
    val tStart = Task('start)
    val t1 = new Task('t1, "t1", 3, Some("A"))
    val t2 = new Task('t2, "t2", 4, Some("B"))
    val t3 = new Task('t3, "t3", 1.5, Some("C"))
    val tEnd = Task('end)

    val deps = List(
      tStart -> t1, tStart -> t2, tStart -> t3,
      t1 -> tEnd, t2 -> tEnd, t3 -> tEnd)

    val tasks = List(tStart, t1, t2, t3, tEnd)

    val sch = Schedule.make(tasks, deps)
    implicit val iSched = new MatchingSchedule(sch)

    // Here's our intended schedule:
    // [id, duration, resource]
    // 
    // [start,0]
    //    +---[t1, 3,     A]-+
    //    +-[t2, 4,       B]-+
    //    +-----[t3, 1.5, C]-+
    //                       \[end,0]

    t1 should endRightBefore (tEnd)
    t2 should endRightBefore (tEnd)
    t3 should endRightBefore (tEnd)
    tStart should endSomeTimeBefore (t1)
    tStart should endRightBefore (t2)
    tStart should endSomeTimeBefore (t3)
  }

  it should "schedule the latest-ending tasks first "+
  		"(2 - latest-ending task before the others)" in {
    val tStart = Task('start)
    val t1 = new Task('t1, "t1", 4, Some("A"))
    val t2 = new Task('t2, "t2", 3, Some("B"))
    val t3 = new Task('t3, "t3", 1.5, Some("C"))
    val tEnd = Task('end)

    val deps = List(
      tStart -> t1, tStart -> t2, tStart -> t3,
      t1 -> tEnd, t2 -> tEnd, t3 -> tEnd)

    val tasks = List(tStart, t1, t2, t3, tEnd)

    val sch = Schedule.make(tasks, deps)
    implicit val iSched = new MatchingSchedule(sch)

    // Here's our intended schedule:
    // [id, duration, resource]
    // 
    // [start,0]
    //    +-[t1, 4,       A]-+
    //    +---[t2, 3,     B]-+
    //    +-----[t3, 1.5, C]-+
    //                       \[end,0]

    t1 should endRightBefore (tEnd)
    t2 should endRightBefore (tEnd)
    t3 should endRightBefore (tEnd)
    tStart should endRightBefore (t1)
    tStart should endSomeTimeBefore (t2)
    tStart should endSomeTimeBefore (t3)
  }

  it should "schedule the latest-ending tasks first "+
  		"(3 - latest-ending task after the others)" in {
    val tStart = Task('start)
    val t1 = new Task('t1, "t1", 1.5, Some("A"))
    val t2 = new Task('t2, "t2", 3, Some("B"))
    val t3 = new Task('t3, "t3", 4, Some("C"))
    val tEnd = Task('end)

    val deps = List(
      tStart -> t1, tStart -> t2, tStart -> t3,
      t1 -> tEnd, t2 -> tEnd, t3 -> tEnd)

    val tasks = List(tStart, t1, t2, t3, tEnd)

    val sch = Schedule.make(tasks, deps)
    implicit val iSched = new MatchingSchedule(sch)

    // Here's our intended schedule:
    // [id, duration, resource]
    // 
    // [start,0]
    //    +-----[t1, 1.5, C]-+
    //    +---[t2, 3,     B]-+
    //    +-[t3, 4,       A]-+
    //                       \[end,0]

    t1 should endRightBefore (tEnd)
    t2 should endRightBefore (tEnd)
    t3 should endRightBefore (tEnd)
    tStart should endSomeTimeBefore (t1)
    tStart should endSomeTimeBefore (t2)
    tStart should endRightBefore (t3)
  }
  
  it should "work even with no dependencies and non-conflicting tasks" in {
    val t1 = new Task('t1, "t1", 3, Some("A"))
    val t2 = new Task('t2, "t2", 4, Some("B"))
    val t3 = new Task('t3, "t3", 1.5, Some("C"))

    val deps = Nil
    val tasks = List(t1, t2, t3)
    val sch = Schedule.make(tasks, deps)
    
    sch.end(t1) should equal (sch.end(t2))
    sch.end(t2) should equal (sch.end(t3))
  }
  
  it should "work even with no dependencies and conflicting tasks" in {
    val t1 = new Task('t1, "t1", 3, Some("Alice"))
    val t2 = new Task('t2, "t2", 4, Some("Alice"))
    val t3 = new Task('t3, "t3", 1.5, Some("Alice"))

    val deps = Nil
    val tasks = List(t1, t2, t3)
    val sch = Schedule.make(tasks, deps)
    
    val earliest = sch.earliestStart(tasks)
    val latest = sch.latestEnd(tasks)
    (latest - earliest) should equal (3 + 4 + 1.5)
  }
  
  it should "not schedule a task before scheduling all its follow-on tasks" in {
    // Here's our intended schedule:
    // [id, duration, resource]
    // 
    // [start,0]
    //    +-[t1, 4,       A]-+
    //    |                  \-[t2, 4,       B]-+
    //    \--------------------------[t3, 2, C]-+
    //                                          \[end,0]
    // It should not schedule start
    // before scheduling t1, even though t3 has a
    // later start.

    val tStart = Task('start)
    val t1 = new Task('t1, "t1", 4, Some("A"))
    val t2 = new Task('t2, "t2", 4, Some("B"))
    val t3 = new Task('t3, "t3", 2, Some("C"))
    val tEnd = Task('end)

    val deps = List(
      tStart -> t1, t1 -> t2, t2 -> tEnd,
      tStart -> t3, t3 -> tEnd)

    val tasks = List(tStart, t1, t2, t3, tEnd)

    val sch = Schedule.make(tasks, deps)
    implicit val iSched = new MatchingSchedule(sch)

    t2 should endRightBefore (tEnd)
    t3 should endRightBefore (tEnd)
    t1 should endRightBefore (t2)
    tStart should endRightBefore (t1)
    tStart should endSomeTimeBefore (t3)
  }

  it should "schedule many tasks in the right order according to dependencies and resources" in {
    val tStart = Task('start)
    val a1 = new Task('a1, "a1", 3, Some("C"))
    val a2 = new Task('a2, "a2", 4, Some("D"))
    val a3 = new Task('a3, "a3", 1.5, Some("E"))
    val b1 = new Task('b1, "b1", 5, Some("A"))
    val b2 = new Task('b2, "b2", 5, Some("B"))
    val c1 = new Task('c1, "c1", 2, Some("B"))
    val c2 = new Task('c2, "c2", 4, Some("B"))
    val c3 = new Task('c3, "c3", 5, Some("A"))
    val c4 = new Task('c4, "c4", 2.5, Some("C"))
    val tEnd = Task('end)

    val deps = List(
      tStart -> a1, tStart -> a2, tStart -> a3,
      tStart -> c1, tStart -> c2,
      a1 -> b1, a2 -> b1, a3 -> b1,
      b1 -> b2,
      c1 -> c3, c2 -> c3,
      c3 -> c4,
      b2 -> tEnd, c4 -> tEnd)

    val tasks = List(tStart, a1, a2, a3, b1, b2, c1, c2, c3, c4, tEnd)

    val sch = Schedule.make(tasks, deps)
    implicit val iSched = new MatchingSchedule(sch)

    // Here's our intended schedule:
    // [id, duration, resource]
    // 
    // [start,0]
    //    +---[a1, 3,     C]-+
    //    +-[a2, 4,       D]-+
    //    +-----[a3, 1.5, E]-+
    //    |                  \[b1, 5,   A]--------\
    //    |                                       +[b2, 5,        B]\
    //    +---------------------[c1, 2, B]\                         |
    //    |                               +[c3, 5,   A]\            |
    //    |                               |            +[c4, 2.5, C]\
    //    \----------[c2, 4,  B]----------/                         |
    //                                                              +[end,0]

    // The sequence b1, b2, tEnd

    b2 should endRightBefore (tEnd)
    c4 should endRightBefore (tEnd)

    // The sequence tStart, c1, c2, c3, c4, tEnd, including resource conflict c1, c2

    c3 should endRightBefore (c4)
    val laterOfC1AndC2 = if (sch.end(c1) > sch.end(c2)) c1 else c2
    val earlierOfC1AndC2 = if (sch.start(c1) < sch.start(c2)) c1 else c2
    laterOfC1AndC2 should endRightBefore (c3)
    earlierOfC1AndC2 should endRightBefore (laterOfC1AndC2)
    sch.end(tStart) should be < sch.start(c1)
    sch.end(tStart) should be < sch.start(c2)

    // Resource conflict b1, c3

    b1 should endRightBefore (c3)

    // The sequence tStart, a1, a2, a3, b1

    a1 should endRightBefore (b1)
    a2 should endRightBefore (b1)
    a3 should endRightBefore (b1)
    sch.end(tStart) should be < sch.start(a1)
    tStart should endRightBefore (a2)
    sch.end(tStart) should be < sch.start(a3)
    
    // println(sch.roughInfo)
  }

}