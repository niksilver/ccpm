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
  def +(t: Task, when: Double): Schedule = new Schedule(starts + (t -> when))

  /**
   * Get the start time of a given task.
   */
  def start(t: Task): Double = try { starts(t) } catch {
    case e: NoSuchElementException => throw new UnknownTaskException(t.toString)
  }

  /**
   * Get the end time of a given task.
   */
  def end(t: Task): Double = try { starts(t) + t.duration } catch {
    case e: NoSuchElementException => throw new UnknownTaskException(t.toString)
  }

  /**
   * Get the end time of a given task, based on half its duration.
   */
  def halfEnd(t: Task): Double = { starts(t) + t.halfDuration }

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
    if (allLaterTasks.isEmpty) {
      new Schedule(starts + (t -> 0.0))
    } else {
      val earliestStart = allLaterTasks map { starts(_) } reduce { Math.min(_, _) }
      val tStart = earliestStart - t.halfDuration
      new Schedule(starts + (t -> tStart))
    }
  }
}
