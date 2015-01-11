package org.pigsaw.ccpm

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

class ScheduleTestBasics extends FlatSpec with Matchers with ScheduleMatchers {
  
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
    sch.halfEnd(t0) should equal (5 + t0.duration / 2)
    sch.halfEnd(t1) should equal (6 + t1.duration / 2)
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

  it should "be true if the new task is zero duration, and sits in the middle of a resource-conflict" in {
    val t99 = Task('t99, "My 99", 5, Some("Alice"))
    val t01 = Task('t01, "My first", 0, Some("Alice"))
    val sch = new Schedule() + (t99, 20)
    // Remember we're working with half-durations
    sch.resourceConflicts(t01, 21) should be (true)
  }

  it should "be false if existing task is zero duration but otherwise conflicts at the start" in {
    val t99 = Task('t99, "My 99", 0, Some("Alice"))
    val t01 = Task('t01, "My first", 5, Some("Alice"))
    val sch = new Schedule() + (t99, 20)
    // Remember we're working with half-durations
    sch.resourceConflicts(t01, 20) should be (false)
  }

  it should "be false if existing task is zero duration but otherwise conflicts at the end" in {
    val t99 = Task('t99, "My 99", 0, Some("Alice"))
    val t01 = Task('t01, "My first", 5, Some("Alice"))
    val sch = new Schedule() + (t99, 20)
    // Remember we're working with half-durations
    sch.resourceConflicts(t01, 17.5) should be (false)
  }

  it should "be true if existing task is zero duration, and sits in the middle of a resource-conflict" in {
    val t99 = Task('t99, "My 99", 0, Some("Alice"))
    val t01 = Task('t01, "My first", 5, Some("Alice"))
    val sch = new Schedule() + (t99, 20)
    // Remember we're working with half-durations
    sch.resourceConflicts(t01, 19) should be (true)
  }

  it should "be false if there's a clear overlap but different resources" in {
    val t99 = Task('t99, "My 99", 5, Some("Alice"))
    val t01 = Task('t01, "My first", 5, Some("Bob"))
    val sch = new Schedule() + (t99, 20)
    // Remember we're working with half-durations
    sch.resourceConflicts(t01, 21) should be (false)
  }

  it should "be false if there's a clear overlap and both have no resources" in {
    val t99 = Task('t99, "My 99", 5, None)
    val t01 = Task('t01, "My first", 5, None)
    val sch = new Schedule() + (t99, 20)
    // Remember we're working with half-durations
    sch.resourceConflicts(t01, 21) should be (false)
  }

  "Schedule.latestStart" should "work if no tasks scheduled (1)" in {
    val sch = new Schedule()
    val t = Task('t, "My first", 5, Some("Alice"))
    sch.latestStart(t, 20) should equal (17.5)
  }

  it should "work if no tasks scheduled (2 - to avoid faking)" in {
    val sch = new Schedule()
    val t = Task('t, "My first", 5, Some("Alice"))
    sch.latestStart(t, 15) should equal (12.5)
  }

  it should "work if several resource-conflicting tasks scheduled" in {
    val t1 = Task('t1, "My one", 5, Some("Alice"))
    val t2 = Task('t2, "My two", 5, Some("Alice"))
    val t3 = Task('t3, "My three", 5, Some("Alice"))
    val t4 = Task('t4, "My four", 5, Some("Alice"))
    val sch = new Schedule() +
      (t4, 20) + // Runs 20 - 22.5
      (t3, 16.5) + // Runs 16.5 - 18
      (t2, 10) // Runs 10 - 12.5
    sch.latestStart(t1, 23) should equal (14.0)
  }

  it should "schedule before the latest start, even if there's a gap in tasks later" in {
    val t1 = Task('t1, "My one", 5, Some("Alice"))
    val t2 = Task('t2, "My two", 5, Some("Alice"))
    val t3 = Task('t3, "My three", 5, Some("Alice"))
    val t4 = Task('t4, "My four", 5, Some("Alice"))
    val sch = new Schedule() +
      (t4, 20) + // Runs 20 - 22.5
      (t3, 16.5) + // Runs 16.5 - 18
      (t2, 10) // Runs 10 - 12.5
    sch.latestStart(t1, 13) should equal (7.5)
  }
  
  "adjustStart" should "adjust start times to given base" in {
    val t1 = Task('t1, "My first", 5, Some("Alice"))
    val t2 = Task('t2, "My second", 4, Some("Alice"))
    val sch1 = Schedule.make(List(t1, t2), Nil)
    val schAdjusted = sch1.adjustStart(0)

    val tasks = List(t1, t2)
    schAdjusted.earliestStart(tasks) should equal (0)
    schAdjusted.latestHalfEnd(tasks) should equal (4.5)
  }
  
  "adjacentTasks" should "be empty if no tasks scheduled" in {
    val sch = new Schedule()
    sch.adjacentTasks should be (Nil)
  }
  
  it should "be empty if no tasks are adjacent" in {
    val t1 = Task('t1, "My first", 5, Some("Alice"))
    val t2 = Task('t2, "My second", 4, Some("Alice"))
    val sch = new Schedule() + (t1, 0) + (t2, 20)
    sch.adjacentTasks should be (Nil)
  }
  
  it should "show one adjacent pair if there are two tasks and they're adjacent" in {
    val t1 = Task('t1, "My first", 5, Some("Alice"))
    val t2 = Task('t2, "My second", 4, Some("Alice"))
    val sch = new Schedule() + (t1, 0) + (t2, 2.5)
    sch.adjacentTasks should be (Seq((t1, t2)))
  }
  
  it should "ignore zero-length tasks" in {
    val t1 = Task('t1, "My first", 5, None)
    val t2 = Task('t2, "Milestone", 0, None)
    val t3 = Task('t3, "My second", 4, None)
    val sch = new Schedule() + (t1, 0) + (t2, 2.5) + (t3, 2.5)
    sch.adjacentTasks should be (Seq((t1, t3)))
  }
  
  it should "not say that a zero-length task is adjacent to itself" in {
    val t1 = Task('t1, "My first", 0, None)
    val sch = new Schedule() + (t1, 0)
    sch.adjacentTasks should be (Nil)
  }
  
  it should "return multiple pairs when appropriate" in {
    val t1 = Task('t1, "My first", 5, None)
    val t2 = Task('t2, "My end", 2, None)
    val t3 = Task('t3, "My parallel first", 4, None)
    val sch = new Schedule() + (t1, 0) + (t2, 2.5) + (t3, 0.5)
    sch.adjacentTasks should contain ((t1, t2))
    sch.adjacentTasks should contain ((t3, t2))
    sch.adjacentTasks.length should equal (2)
  }
  
  "resourceAdjacentTasks" should "ignore adjacent tasks with different resources" in {
    val t1 = Task('t1, "My first", 5, Some("Alice"))
    val t2 = Task('t2, "My second", 4, Some("Bob"))
    val sch = new Schedule() + (t1, 0) + (t2, 2.5)
    sch.resourceAdjacentTasks should be (Nil)
  }
  
  it should "return adjacent tasks with same resources" in {
    val t1 = Task('t1, "My first", 5, Some("Alice"))
    val t2 = Task('t2, "My second", 4, Some("Alice"))
    val sch = new Schedule() + (t1, 0) + (t2, 2.5)
    sch.resourceAdjacentTasks should be (Seq((t1, t2)))
  }
  
  it should "return adjacent tasks with same resources even if not all adjacent tasks have same resources" in {
    val t1 = Task('t1, "My first", 5, None)
    val t2 = Task('t2, "My end", 2, Some("Bob"))
    val t3 = Task('t3, "My parallel first", 4, Some("Bob"))
    val sch = new Schedule() + (t1, 0) + (t2, 2.5) + (t3, 0.5)
    sch.resourceAdjacentTasks should equal (Seq((t3, t2)))
  }
}
