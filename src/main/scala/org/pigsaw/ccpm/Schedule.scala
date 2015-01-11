package org.pigsaw.ccpm

import scala.annotation.tailrec
import scala.collection.TraversableLike

/**
 * Start times for tasks
 */
class Schedule(private val starts: Map[Task, Double] = Nil.toMap) {

  import Schedule._

  /**
   * The tasks in the schedule.
   */
  lazy val tasks: Iterable[Task] = starts.keys

  /**
   * The tasks in the schedule.
   */
  lazy val taskSet: Set[Task] = starts.keySet

  /**
   * Is this task scheduled?
   */
  def isScheduled(t: Task): Boolean = { taskSet contains t }

  /**
   * Add a task and its start time to the schedule, and
   * return the new schedule.
   */
  def +(t: Task, when: Double): Schedule = new Schedule(starts + (t -> when))

  /**
   * Get the start time of a given task.
   */
  def start(t: Task): Double = starts.get(t) match {
    case Some(start) => start
    case None => throw new UnknownTaskException(t.toString)
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
   * If the task `t` starts at time `tStart`, does it
   * resource-conflict with any of the tasks scheduled so far?
   */
  def resourceConflicts(t: Task, tStart: Double): Boolean = {
    val tHalfEnd = tStart + t.halfDuration
    def conflictsWith(t2: Task) = {
      (t.duration > 0 && start(t2) < tHalfEnd && tHalfEnd <= halfEnd(t2)) ||
        (t.duration > 0 && start(t2) <= tStart && tStart < halfEnd(t2)) ||
        (t.duration == 0 && start(t2) < tStart && tStart < halfEnd(t2)) ||
        (t2.duration == 0 && tStart < start(t2) && start(t2) < tHalfEnd)
    }
    tasks filter { t.sameResource(_) } exists { conflictsWith(_) }
  }

  /**
   * Return the latest possible start for a task which will have no
   * resource conflicts with currently-scheduled tasks, and which
   * does not allow the task to run later that `tLatest`.
   */
  def latestStart(t: Task, latest: Double): Double = {
    val firstGuess = latest - t.halfDuration
    val otherGuesses = tasks map { start(_) - t.halfDuration } filter { _ < firstGuess }
    val allGuesses = List(firstGuess) ++ otherGuesses
    val goodGuesses = allGuesses filter { !resourceConflicts(t, _) }
    val bestGuess = goodGuesses reduce { Math.max(_, _) }
    bestGuess
  }

  /**
   * Get the earliest start time of all the given tasks.
   */
  def earliestStart(ts: Iterable[Task]): Double = ts map { start(_) } reduce { Math.min(_, _) }

  /**
   * Get the latest half-end time of all the given tasks.
   */
  def latestHalfEnd(ts: Iterable[Task]): Double = ts map { halfEnd(_) } reduce { Math.max(_, _) }

  /**
   * Schedule a task as late as possible avoiding resource conflicts.
   * The latest half-end time for a task will be used as a backstop,
   * or (if there are no tasks scheduled) it will get
   * the `defaultStart` time.
   */
  def schedule(t: Task): Schedule = schedule(t, Nil)

  /**
   * Schedule a test before any given others (specified by `laters`),
   * and without a resource conflict.
   * If `laters` is empty then the latest half-end time for a task
   * will be used as a backstop.
   * The task `t` will start as late as it can to fit these requirements.
   * If `laters` is empty and there are no tasks scheduled already then
   * this first task will get the `defaultStart` time.
   */
  def schedule(t: Task, laters: Seq[Task]): Schedule = {
    if (laters.isEmpty && starts.isEmpty) {
      new Schedule(starts + (t -> defaultStart))
    } else if (laters.isEmpty && starts.nonEmpty) {
      val mustntRunInto = latestHalfEnd(tasks)
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
  def schedule(ts: Seq[Task], deps: Seq[(Task, Task)]): Schedule = {
    // First schedule the end tasks, then follow on from there
    val g = new Graph(deps)
    val ends = if (g.ends.nonEmpty) g.ends else ts
    val sch = scheduleEnds(ends)
    val remaining = ts filter { !sch.isScheduled(_) }
    sch.scheduleFollowOns(remaining, deps)
  }

  // Schedule tasks to be at the end of the plan
  @tailrec
  private def scheduleEnds(ends: Seq[Task]): Schedule =
    ends match {
      case Nil => this
      case t :: Nil => schedule(t)
      case t :: tOthers => schedule(t).scheduleEnds(tOthers)
    }

  @tailrec
  private def scheduleFollowOns(ts: Seq[Task], deps: Seq[(Task, Task)]): Schedule = {
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
      val sch = schedule(t, laters)
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
      val size = t.halfDuration * scale
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
    adjacentTasks({ (_, _) => true})
  }
  
  // Find adjacent tasks where the tasks also meet
  // a given condition.
  //
  private def adjacentTasks(cond: (Task,Task) => Boolean): Seq[Tuple2[Task, Task]] = {
    for {
      (task1, start1) <- starts.toSeq
      (task2, start2) <- (starts filter { td => cond(task1, td._1) })
      if task1.duration != 0 && task2.duration != 0 && halfEnd(task1) == start2
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
    adjacentTasks( _.sameResource(_))
    
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
  def make(ts: Seq[Task], deps: Seq[(Task, Task)]): Schedule =
    (new Schedule()).schedule(ts, deps)
}
