package org.pigsaw.ccpm

import java.util.NoSuchElementException

/**
 * Start times for tasks
 */
class Schedule(starts: Map[Task, Double] = Nil.toMap) {
  
  /**
   * Add a task and its start time to the schedule, and
   * return the new schedule.
   */
  def +(t: Task, when: Int): Schedule = new Schedule(starts + (t -> when))
  
  /**
   * Get the start time of a given task.
   */
  def start(t: Task): Double = try { starts(t) } catch {
    case e: NoSuchElementException => throw new UnknownTaskException(t.toString)
  }
  
  /**
   * Schedule a task.
   * The first task will get an arbitrary start time.
   * A task with a resource conflict will start as early as it can before the
   * resource becomes available.
   */
  def schedule(t: Task): Schedule = {
    val conflicted = starts.keys filter { _.resource == t.resource }
    if (conflicted.size > 0) {
      val earliestStart = conflicted map { starts(_) } reduce { Math.min(_, _) }
      val tStart = earliestStart - t.duration
      new Schedule(starts + (t -> tStart))
    }
    else {
      new Schedule(starts + (t -> 0))
    }
  }
  
  /**
   * Schedule a test before a given other one.
   */
  def schedule(t: Task, later: Task): Schedule = {
    val tStart = starts(later) - t.duration
    new Schedule(starts + (t -> tStart))
  }
}
