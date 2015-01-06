package org.pigsaw.ccpm

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
   * Schedule a task as late as possible avoiding resource conflicts.
   */
  def schedule(t: Task): Schedule = schedule(t, Nil)

  /**
   * Schedule a test before any given others (specified by `laters`),
   * and without a resource conflict.
   * If `laters` is empty then the latest half-end time for a task
   * will be used as a backstop.
   * The task `t` will start as late as it can to fit these requirements.
   * If `laters` is empty and there are no tasks scheduled already then
   * this first task will get an arbitrary start time.
   */
  def schedule(t: Task, laters: Seq[Task]): Schedule = {
    if (laters.isEmpty && starts.isEmpty) {
      new Schedule(starts + (t -> defaultStart))
    } else if (laters.isEmpty && starts.nonEmpty) {
      val mustntRunInto = tasks map { halfEnd(_) } reduce { Math.max(_, _) }
      val tStart = latestStart(t, mustntRunInto)
      new Schedule(starts + (t -> tStart))
    } else {
      val mustntRunInto = laters map { start(_) } reduce { Math.min(_, _) }
      val tStart = latestStart(t, mustntRunInto)
      new Schedule(starts + (t -> tStart))
    }
  }

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
    def sameResources(t2: Task) = { t.resource.nonEmpty && t.resource == t2.resource }
    tasks filter { sameResources(_) } exists { conflictsWith(_) }
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
   * Schedule some tasks respecting resource conflicts and dependencies.
   */
  def schedule(ts: Seq[Task], deps: Seq[(Task, Task)]): Schedule = {
    // First schedule the end tasks, then follow on from there
    val g = new Graph(deps)
    val ends = g.ends
    val sch = scheduleEnds(ends)
    val remaining = sch.tasks.toSeq
    sch.scheduleFollowOns(remaining, deps)
  }

  // Schedule tasks to be at the end of the plan
  private def scheduleEnds(ends: Seq[Task]): Schedule = ends match {
    case Nil => this
    case t :: Nil => schedule(t)
    case t :: tOthers => schedule(t).scheduleEnds(tOthers)
  }

  private def scheduleFollowOns(ts: Seq[Task], deps: Seq[(Task, Task)]): Schedule = {
    // Of all the earlier/later dependencies
    // (a) pick out all those where the later part has been scheduled but
    //     the earlier part hasn't. (Dangling pairs)
    // (b) Then remove all those pairs where the earlier part has a later
    //     dependency elsewhere that's not been scheduled. (Leaving so-called stable pairs)
    // (c) Then narrow it down any where the later part starts
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

}

/**
 * Default `Schedule` values.
 */
object Schedule {

  /**
   * The time a task will start if no other guidance is available.
   */
  val defaultStart = 0.0

}
