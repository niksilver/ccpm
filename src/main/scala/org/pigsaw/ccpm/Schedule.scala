package org.pigsaw.ccpm

import java.util.NoSuchElementException

/**
 * Start times for tasks
 */
class Schedule(starts: Map[Task, Int] = Nil.toMap) {
  
  /**
   * Add a task and its start time to the schedule, and
   * return the new schedule.
   */
  def +(t: Task, when: Int): Schedule = new Schedule(starts + (t -> when))
  
  /**
   * Get the start time of a given task.
   */
  def start(t: Task): Int = try { starts(t) } catch {
    case e: NoSuchElementException => throw new UnknownTaskException(t.toString)
  }
  
  /**
   * Schedule a task. The first task will get an arbitrary start time.
   */
  def schedule(t: Task): Schedule = new Schedule(Map(t -> 0))
}
