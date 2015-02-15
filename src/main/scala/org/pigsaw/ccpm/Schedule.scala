package org.pigsaw.ccpm

import scala.annotation.tailrec
import scala.collection.TraversableLike

/**
 * Start times for tasks
 */
class Schedule(protected[ccpm] val starts: Map[Period, Double] = Nil.toMap) {

  import Schedule._

  /**
   * The tasks in the schedule.
   */
  lazy val tasks: Iterable[Task] = starts.keys collect { case t: Task => t }

  /**
   * The tasks in the schedule.
   */
  lazy val taskSet: Set[Task] = starts.keySet collect { case t: Task => t }

  // All the starts of just the tasks
  private lazy val taskStarts = starts collect { case (t: Task, s: Double) => (t, s) }

  /**
   * Is this period scheduled?
   */
  def isScheduled(t: Task): Boolean = { taskSet contains t }

  /**
   * Add a task or buffer and its start time to the schedule, and
   * return the new schedule.
   */
  def +(p: Period, when: Double): Schedule = new Schedule(starts + (p -> when))

  /**
   * Remove a task or buffer rom the schedule, and return the resulting
   * new schedule.
   */
  def -(p: Period): Schedule = if (starts.keySet contains p) {
    new Schedule(starts - p)
  } else {
    throw new UnknownPeriodException(s"Period $p not in schedule")
  }

  /**
   * Return a new schedule which is the same as this, but with a given
   * task's start being changed.
   */
  def changing(p: Period, d: Double) =
    if (starts isDefinedAt p) {
      new Schedule(starts + (p -> d))
    } else {
      throw new UnknownTaskException(s"Could not change task $p because it does not exist in the schedule")
    }

  /**
   * Get the start time of a given period.
   */
  def start(p: Period): Double = starts.get(p) match {
    case Some(start) => start
    case None => throw new UnknownTaskException(p.toString)
  }

  /**
   * Get the end time of a given task.
   */
  def end(t: Task): Double = start(t) + t.duration

  /**
   * If the task `t` starts at time `tStart`, does it
   * resource-conflict with any of the tasks scheduled so far?
   */
  def resourceConflicts(t: Task, tStart: Double): Boolean = {
    tasks filter { t.sameResource(_) } exists { overlaps(t, tStart, _) }
  }

  /**
   * If task `t` started at `tStart`, does it overlap with `t2`?
   */
  def overlaps(t: Task, tStart: Double, t2: Task): Boolean = {
    val tEnd = tStart + t.duration
    (t.duration > 0 && start(t2) < tEnd && tEnd <= end(t2)) ||
      (t.duration > 0 && start(t2) <= tStart && tStart < end(t2)) ||
      (t.duration == 0 && start(t2) < tStart && tStart < end(t2)) ||
      (t2.duration == 0 && tStart < start(t2) && start(t2) < tEnd)
  }

  /**
   * Get the end times of all tasks between the given lower and upper bound inclusive.
   */
  def endsBetween(lower: Double, upper: Double): Set[Double] = {
    val allEnds = starts map { case (p, s) => s + p.duration }
    (allEnds filter { s => lower <= s && s <= upper }).toSet
  }

  /**
   * Return the latest possible start for a task which will have no
   * resource conflicts with currently-scheduled tasks, and which
   * does not allow the task to run later that `latest`.
   */
  def latestStart(t: Task, latest: Double): Double = {
    val firstGuess = latest - t.duration
    val otherGuesses = tasks map { start(_) - t.duration } filter { _ < firstGuess }
    val allGuesses = List(firstGuess) ++ otherGuesses
    val goodGuesses = allGuesses filter { !resourceConflicts(t, _) }
    val bestGuess = goodGuesses.max
    bestGuess
  }

  /**
   * Get the earliest start time of all the given tasks.
   */
  def earliestStart(ts: Iterable[Task]): Double = (ts map start).min

  /**
   * Get the latest end time of all the given tasks.
   */
  def latestEnd(ts: Iterable[Task]): Double = (ts map end).max

  /**
   * Schedule a task as late as possible avoiding resource conflicts.
   * The latest end time for a task will be used as a backstop,
   * or (if there are no tasks scheduled) it will get
   * the `defaultStart` time.
   */
  def schedule(t: Task): Schedule = scheduleBefore(t, Set())

  /**
   * Schedule a test before any given others (specified by `laters`),
   * and without a resource conflict.
   * If `laters` is empty then the latest end time for a task
   * will be used as a backstop.
   * The task `t` will start as late as it can to fit these requirements.
   * If `laters` is empty and there are no tasks scheduled already then
   * this first task will get the `defaultStart` time.
   */
  def scheduleBefore(t: Task, laters: Set[Task]): Schedule = {
    if (laters.isEmpty && starts.isEmpty) {
      new Schedule(starts + (t -> defaultStart))
    } else if (laters.isEmpty && starts.nonEmpty) {
      val mustntRunInto = latestEnd(tasks)
      val tStart = latestStart(t, mustntRunInto)
      new Schedule(starts + (t -> tStart))
    } else {
      val mustntRunInto = earliestStart(laters)
      val tStart = latestStart(t, mustntRunInto)
      new Schedule(starts + (t -> tStart))
    }
  }

  /**
   * Schedule some tasks respecting resource conflicts and dependencies.
   */
  def schedule(ts: Set[Task], deps: Set[(Task, Task)]): Schedule = {
    // First schedule the end tasks, then follow on from there
    val g = new Graph(deps)
    val ends = if (g.ends.nonEmpty) g.ends else ts
    val sch = scheduleEnds(ends)
    val remaining = ts filter { !sch.isScheduled(_) }
    sch.scheduleFollowOns(remaining, deps)
  }

  // Schedule tasks to be at the end of the plan
  @tailrec
  private def scheduleEnds(ends: Set[Task]): Schedule =
    ends.size match {
      case 0 => this
      case 1 => schedule(ends.head)
      case _ => { val (t, tOthers) = ends.splitAt(1); schedule(t.head).scheduleEnds(tOthers) }
    }

  @tailrec
  private def scheduleFollowOns(ts: Set[Task], deps: Set[(Task, Task)]): Schedule = {
    // Of all the earlier/later dependencies
    // (a) pick out all those where the later part has been scheduled but
    //     the earlier part hasn't. (Dangling pairs)
    // (b) Then remove all those pairs where the earlier part has a later
    //     dependency elsewhere that's not been scheduled. (Leaving so-called stable pairs)
    // (c) Then narrow it down to any where the later part starts
    //     latest of all.
    // (d) Then schedule that, and repeat
    val danglingPairs = deps filter { pair => isScheduled(pair._2) && !isScheduled(pair._1) }
    def hasAnUnscheduledLater(t: Task) = { deps exists { pair => pair._1 == t && !isScheduled(pair._2) } }
    val stablePairs = danglingPairs filterNot { pair => hasAnUnscheduledLater(pair._1) }
    if (stablePairs.isEmpty) {
      this
    } else {
      def laterStartingPair(p1: (Task, Task), p2: (Task, Task)) =
        if (start(p1._2) > start(p2._2)) p1 else p2
      val latestStartingPair = stablePairs reduce { laterStartingPair(_, _) }
      val t = latestStartingPair._1
      val laters = stablePairs filter { _._1 == t } map { _._2 }
      val sch = scheduleBefore(t, laters)
      val remaining = ts filter { _ != t }
      sch.scheduleFollowOns(remaining, deps)
    }
  }

  /**
   * Create a new schedule which is just like this one, but
   * all the start times have been shifted so that the earliest
   * time is as given (`base`).
   */
  def adjustStart(base: Double): Schedule = {
    val earliest = earliestStart(tasks)
    val starts2 = starts map { kv => (kv._1, kv._2 - earliest + base) }
    new Schedule(starts2)
  }

  /**
   * Describe the schedule as a `String`.
   */
  def roughInfo: String = adjustStart(0).roughInfo0

  // Here the earliest start is at 0
  private def roughInfo0: String = {
    val scale = 4
    val maxId = tasks map { _.id.toString.length } reduce { Math.max(_, _) }
    val sortedTasks = tasks.toSeq sortBy { _.id.toString }
    val ts = sortedTasks map { t =>
      val label = force(t.id.toString, maxId, " ")
      val size = t.duration * scale
      val block = asBlock(t, size.toInt)
      val indentSize = (start(t) * scale).toInt
      val indent = " " * indentSize
      label + indent + "  " + block
    }
    ts.mkString("\n")
  }

  private def asBlock(t: Task, width: Int): String = {
    val msg = t.resource.getOrElse("")
    "[" + force(msg, width - 2, "-") + "]"
  }

  private def force(str: String, size: Int, padding: String) = {
    //str.padTo(size, padding).take(size).mkString
    (str + (padding * size)).take(size).mkString
  }

  /**
   * Get all pairs of tasks where the half-end time of the first
   * is the start time of the second. This is regardless of
   * whether there is any dependency between them.
   * Zero-length tasks are entirely ignored.
   */
  def adjacentTasks: Seq[Tuple2[Task, Task]] = {
    def cond(t1: Task, t2: Task) = true
    adjacentTasks({ (_, _) => true })
  }

  // Find adjacent tasks where the tasks also meet
  // a given condition.
  //
  private def adjacentTasks(cond: (Task, Task) => Boolean): Seq[Tuple2[Task, Task]] = {
    for {
      (task1, start1) <- taskStarts.toSeq
      (task2, start2) <- (taskStarts filter { td => cond(task1, td._1) })
      if task1.duration != 0 && task2.duration != 0 && end(task1) == start2
    } yield (task1, task2)
  }

  /**
   * Get all pairs of tasks where the half-end time of the first
   * is the start time of the second and they use the same
   * resources. This is regardless of
   * whether there is any other dependency between them.
   * Zero-length tasks are entirely ignored.
   */
  def resourceAdjacentTasks: Seq[Tuple2[Task, Task]] =
    adjacentTasks(_.sameResource(_))

}

/**
 * Default `Schedule` values.
 */
object Schedule {

  /**
   * The time a task will start if no other guidance is available.
   */
  val defaultStart = 0.0

  /**
   * Make a new schedule with the given tasks and dependencies.
   */
  def make(ts: Set[Task], deps: Set[(Task, Task)]): Schedule =
    (new Schedule()).schedule(ts, deps)
}
