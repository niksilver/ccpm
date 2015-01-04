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
      val tOld = conflicted.head
      val oldStart = starts(tOld)
      new Schedule(starts + (t -> (oldStart.toDouble - t.duration)))
    }
    else {
      new Schedule(starts + (t -> 0))
    }
  }
}
