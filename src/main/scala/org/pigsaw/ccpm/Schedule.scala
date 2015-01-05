package org.pigsaw.ccpm

import java.util.NoSuchElementException

/**
 * Start times for tasks
 */
class Schedule(private val starts: Map[Task, Double] = Nil.toMap) {
  
  /** The tasks in the schedule.
   */
  lazy val tasks: Iterable[Task] = starts.keys
  
  /** The tasks in the schedule.
   */
  lazy val taskSet: Set[Task] = starts.keySet

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
  def end(t: Task): Double = start(t) + t.duration

  /**
   * Get the end time of a given task, based on half its duration.
   */
  def halfEnd(t: Task): Double = start(t) + t.halfDuration

  /**
   * Schedule a task as late as possible avoiding resource conflicts.
   */
  def schedule(t: Task): Schedule = schedule(t, Nil)

  /**
   * Schedule a test before any given others,
   * and before any with a resource conflict.
   * The task `t` will start as late as it can to fit these requirements.
   * The first task will get an arbitrary start time.
   */
  def schedule(t: Task, laters: Seq[Task]): Schedule = {
    val resConflicted = starts.keys filter { _.resource == t.resource }
    val allLaterTasks = resConflicted ++ laters
    if (allLaterTasks.isEmpty) {
      println(s"Scheduling task $t at 0.0")
      new Schedule(starts + (t -> 0.0))
    } else {
      val earliestStart = allLaterTasks map { start(_) } reduce { Math.min(_, _) }
      val tStart = earliestStart - t.halfDuration
      println(s"Scheduling task $t at $tStart")
      new Schedule(starts + (t -> tStart))
    }
  }
  
  def resourceConflicts(t: Task, tStart: Double): Boolean = {
    val tHalfEnd = tStart + t.halfDuration
    def conflictsWith(t2: Task) = {
      (t.duration > 0 && start(t2) < tHalfEnd && tHalfEnd <= halfEnd(t2)) ||
      (t.duration > 0 && start(t2) <= tStart && tStart < halfEnd(t2)) ||
      (t.duration == 0 && start(t2) < tStart && tStart < halfEnd(t2)) ||
      (t2.duration == 0 && tStart < start(t2) && start(t2) < tHalfEnd)
    }
    def sameResources(t2: Task) = { t.resource.nonEmpty && t.resource == t2.resource }
    tasks filter { sameResources(_) } exists { conflictsWith(_) }
  }

  /**
   * Schedule some tasks respecting resource conflicts and dependencies.
   */
  def schedule(ts: Seq[Task], deps: Seq[(Task, Task)]): Schedule = {
    // First schedule the end tasks, then follow on from there
    val g = new Graph(deps)
    val ends = g.ends
    val sch = scheduleEnds(ends)
    val schedTs = sch.starts.keySet
    val remaining = ts filterNot { schedTs contains _ }
    sch.scheduleFollowOns(remaining, deps)
  }
  
  private def scheduleEnds(ends: Seq[Task]): Schedule = ends match {
    case Nil => this
    case t :: Nil => schedule(t)
    case t :: tOthers => schedule(t).scheduleEnds(tOthers)
  }
  
  private def scheduleFollowOns(ts: Seq[Task], deps: Seq[(Task, Task)]): Schedule = {
    // Of all the earlier/later dependencies, pick out all those
    // where the later part has been scheduled but the earlier part
    // hasn't. Then pick one of those to schedule, and repeat
    println("----------------")
    val scheduled = taskSet
    val danglingPairs = deps filter { pair => (scheduled contains pair._2) && !(scheduled contains pair._1) }
    val tEarliers = danglingPairs map { _._1 }
    println(s"ts = $ts")
    println(s"Scheduled = $scheduled")
    println(s"danglingPairs = $danglingPairs")
    println(s"tEarliers = $tEarliers")
    if (tEarliers.isEmpty) {
      println("tEarliers is empty; returning this")
      this
    } else {
      val t = tEarliers.head
      println(s"Going to schedule task t = $t")
      val laters = deps filter { _._1 == t } map { _._2 } filter { taskSet contains _ }
      val sch = schedule(t, laters)
      val remaining = ts filter { _ != t }
      println(s"laters = $laters")
      println(s"remaining = $remaining")
      println("Recursing... -------------")
      sch.scheduleFollowOns(remaining, deps)
    }
  }

}
