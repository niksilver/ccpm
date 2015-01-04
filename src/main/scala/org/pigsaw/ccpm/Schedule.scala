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
   * Schedule a task as late as possible avoiding resource conflicts.
   */
  def schedule(t: Task): Schedule = schedule(t, Nil)

  /**
   * Schedule a test before any given others,
   * and before any with a resource conflict.
   * The task `t` will start as late as it can to fit these requirements.
   * The first task will gen an arbitrary start time.
   */
  def schedule(t: Task, laters: Seq[Task]): Schedule = {
    val resConflicted = starts.keys filter { _.resource == t.resource }
    val allLaterTasks = resConflicted ++ laters
    val tStart = allLaterTasks.isEmpty match {
      case true => 0.0
      case false => {
        val allLaterStarts = allLaterTasks map { starts(_) }
        val earliestStart =  allLaterStarts reduce { Math.min(_, _) }
        earliestStart - t.duration
      }
    }
    new Schedule(starts + (t -> tStart))
  }
}
