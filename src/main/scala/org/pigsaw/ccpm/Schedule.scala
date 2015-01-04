package org.pigsaw.ccpm

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
  def start(t: Task) = starts(t)
  
}