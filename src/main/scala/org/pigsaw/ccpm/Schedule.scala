package org.pigsaw.ccpm

import java.util.NoSuchElementException

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
   * Schedule a test before any given others (specified by `laters`),
   * and without a resource conflict.
   * If `laters` is empty then the latest half-end time for a task
   * will be used as a backstop.
   * The task `t` will start as late as it can to fit these requirements.
   * If `laters` is empty and there are no tasks scheduled already then
   * this first task will get an arbitrary start time.
   */
  def schedule(t: Task, laters: Seq[Task]): Schedule = {
    //println(s"Schedule task $t")
    if (laters.isEmpty && starts.isEmpty) {
      //println(s"laters is empty - Scheduling task $t at 0.0")
      new Schedule(starts + (t -> defaultStart))
    } else if (laters.isEmpty && starts.nonEmpty) {
      val mustntRunInto = tasks map { halfEnd(_) } reduce { Math.max(_, _) }
      val tStart = latestStart(t, mustntRunInto)
      new Schedule(starts + (t -> tStart))
    } else {
      val mustntRunInto = laters map { start(_) } reduce { Math.min(_, _) }
      //println(s"mustntRunInto = $mustntRunInto")
      val tStart = latestStart(t, mustntRunInto)
      //println(s"tStart = $tStart")
      new Schedule(starts + (t -> tStart))
    }
  }
  //  def schedule000(t: Task, laters: Seq[Task]): Schedule = {
  //    val resConflicted = starts.keys filter { _.resource == t.resource }
  //    val allLaterTasks = resConflicted ++ laters
  //    if (allLaterTasks.isEmpty) {
  //      println(s"Scheduling task $t at 0.0")
  //      new Schedule(starts + (t -> 0.0))
  //    } else {
  //      val earliestStart = allLaterTasks map { start(_) } reduce { Math.min(_, _) }
  //      val tStart = earliestStart - t.halfDuration
  //      println(s"Scheduling task $t at $tStart")
  //      new Schedule(starts + (t -> tStart))
  //    }
  //  }

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
  def latestStart(t: Task, tLatest: Double): Double = {
    val firstGuess = tLatest - t.halfDuration
    val otherGuesses = tasks map { start(_) - t.halfDuration } filter { _ < firstGuess }
    val allGuesses = List(firstGuess) ++ otherGuesses
    val goodGuesses = allGuesses filter { !resourceConflicts(t, _) }
    val bestGuess = goodGuesses reduce { Math.max(_, _) }
    //    println(s"firstGuess = $firstGuess")
    //    println(s"otherGuesses = $otherGuesses")
    //    println(s"allGuesses = $allGuesses")
    //    println(s"goodGuesses = $goodGuesses")
    println(s"bestGuess for $t = $bestGuess")
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
    val schedTs = sch.starts.keySet
    val remaining = ts filterNot { schedTs contains _ }
    sch.scheduleFollowOns(remaining, deps)
  }

  // Schedule tasks to be at the end of the plan
  private def scheduleEnds(ends: Seq[Task]): Schedule = {
    println("----------- scheduleEnds")
    ends match {
      case Nil => this
      case t :: Nil => schedule(t)
      case t :: tOthers => schedule(t).scheduleEnds(tOthers)
    }
  }

  private def scheduleFollowOns(ts: Seq[Task], deps: Seq[(Task, Task)]): Schedule = {
    // Of all the earlier/later dependencies
    // (a) pick out all those where the later part has been scheduled but
    //     the earlier part hasn't.
    // (b) Then narrow it down any where the later part starts
    //     latest of all.
    // (c) Then schedule that, and repeat
    println("---------------- scheduleFollowOns")
    val scheduled = taskSet
    val danglingPairs = deps filter { pair => (scheduled contains pair._2) && !(scheduled contains pair._1) }
    println(s"ts = $ts")
    println(s"Scheduled = $scheduled")
    println(s"danglingPairs = $danglingPairs")
    if (danglingPairs.isEmpty) {
      println("tEarliers is empty; returning this")
      this
    } else {
      def laterStartingPair(p1: (Task,Task), p2: (Task,Task)) =
        if (start(p1._2) > start(p2._2)) p1 else p2
      val latestStartingPair = danglingPairs reduce { laterStartingPair(_, _) }
      val t = latestStartingPair._1
      println(s"Going to schedule task t = $t which pairs with ${latestStartingPair._2} starting ${start(latestStartingPair._2)}")
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

/**
 * Default `Schedule` values.
 */
object Schedule {

  /**
   * The time a task will start if no other guidance is available.
   */
  val defaultStart = 0.0

}