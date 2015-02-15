package org.pigsaw.ccpm

/**
 * A project plan, which may or may not be a good one.
 */
trait Plan {

  /**
   * Tasks in the plan
   */
  val tasks: Set[Task]

  /**
   * Retrieve a task by its id.
   */
  def task(id: Symbol) = Task.task(tasks, id)

  /**
   * Resources in the plan.
   */
  lazy val resources: Set[String] = (tasks map { _.resource }).flatten.toSet

  /**
   * A list of task pairs `t0 -> t1` where `t0` has to finish
   * before `t1` can start.
   */
  val dependencies: Set[(Task, Task)]

  /**
   * An acyclic graph of the dependencies
   */
  lazy val graph = new Graph(dependencies)

  /**
   * A schedule for this plan.
   */
  lazy val schedule: Schedule = Schedule.make(tasks.toSet, dependencies)
  
  /**
   * Get all the tasks which prevent the given task `t` moving back
   * any further. So these are the tasks which end where `t` starts,
   * and which either use the same resource(s) or which are a
   * dependency of `t`.
   */
  def backingTasks(t: Task): Set[Task] = {
    def backsOntoT(t2: Task): Boolean = { schedule.end(t2) == schedule.start(t) }
    val abuttingPredecessors = graph.predecessors(t) filter { backsOntoT(_) }
    val abuttingResourceMatches =
      tasks filter { backsOntoT(_) } filter { _ != t } filter { _.sameResource(t) }
    abuttingPredecessors ++ abuttingResourceMatches
  }

  /**
   * Get all possible chains for this plan. This includes non-critical chains.
   */
  lazy val chains: Set[Seq[Task]] = {
    val resPairs = schedule.resourceAdjacentTasks
    val newResPairs = resPairs filterNot { pair => graph.hasEdge(pair) }
    val chainedDependencies = dependencies ++ newResPairs
    val chainedGraph = new Graph(chainedDependencies)
    chainedGraph.paths
  }

  /**
   * Get the longest chain.
   */
  lazy val criticalChain: Seq[Task] = {
    if (tasks.size == 0) {
      Seq()
    } else if (tasks.size == 1) {
      Seq(tasks.head)
    } else {
      val longest = chains maxBy { Chain(_).length }
      longest.toSeq
    }
  }

  /**
   * Get all paths that are distinct from the critical chain.
   * Each task will be on exactly one path, and none of the
   * tasks will be on the critical chain.
   * That means that all the tasks in all the non-critical
   * paths and the critical chain combined is exactly all
   * the tasks in the plan, with no repeats.
   */
  def nonCriticalPaths: Set[Seq[Task]] = {
    buildSlices(graph.paths, Set(), criticalChain.toSet)
  }

  /**
   * Given some paths, return the slices found from removing
   * all the excluded tasks. Initially the excluded tasks are
   * those on the critical chain, but as more slices are
   * found so the tasks on those slices are added to the excluded
   * list.
   */
  private def buildSlices(paths: Set[Seq[Task]], acc: Set[Seq[Task]], excluded: Set[Task]): Set[Seq[Task]] = {
    paths.size match {
      case 0 => acc
      case _ => {
        val (path, rest) = paths.splitAt(1)
        val (newAcc, newExcluded) = slicePath(path.head, acc, excluded)
        buildSlices(rest, newAcc, newExcluded)
      }
    }
  }

  /**
   * Given a path, return the slices found from
   * removing all the `excluded` tasks.
   * @param path  The path to slice.
   * @param acc   The accumulated slices found so far
   * @excluded    Tasks to exclude, to leave the slices
   * @returns   A pair: The slices, and the excluded tasks (which are
   *            an amalgam of the tasks in the slices)
   */
  private def slicePath(path: Seq[Task], acc: Set[Seq[Task]], excluded: Set[Task]): (Set[Seq[Task]], Set[Task]) = {
    if (path.isEmpty) {
      (acc, excluded)
    } else {
      val usable = path dropWhile { excluded contains _ }
      val (slice, next) = usable span { t => !(excluded contains t) }
      val newAcc = if (slice.isEmpty) acc else acc + slice
      slicePath(next, newAcc, slice ++: excluded)
    }
  }

  /**
   * Give the task that follows on from this path and which
   * is on the critical chain
   */
  def feedOnCriticalChain(path: Seq[Task]): Option[Task] = {
    val nextTasks = graph.successors(path.last)
    nextTasks find { criticalChain contains _ }
  }

  /**
   * Get all the paths which feed directly into the critical chain.
   */
  def feederPaths: Set[Seq[Task]] = {
    nonCriticalPaths filter { feedOnCriticalChain(_).nonEmpty }
  }

  /**
   * Get the completion buffer (aka project buffer).
   */
  lazy val completionBuffer: Buffer = {
    val id = Buffer.nextId(tasks map { _.id })
    Buffer.make(id, criticalChain)
  }

  /**
   * Get a schedule for this plan, including buffers.
   */
  lazy val bufferedSchedule: Schedule = {
    val lastTask = criticalChain.last
    val lastTaskEnd = schedule.end(lastTask)
    schedule + (completionBuffer, lastTaskEnd)
  }

  /**
   * See how far we can move back a given task.
   * Returns the best distance possible we can move.
   */
  def measureMoveBack(t: Task, max: Double): Double = {
    val tStart = schedule.start(t)
    val preventers = preventsMove(t, tStart - max)
    if (preventers.isEmpty) {
      max
    } else {
      tStart - (preventers map { schedule.end(_) }).max
    }
  }

  /**
   * Move a task back a maximum number of units.
   */
  def moveBack(t: Task, max: Double): Schedule = {
    val tStart = schedule.start(t)
    val actualMove = measureMoveBack(t, max)
    schedule changing (t, tStart - actualMove)
  }
  
  /**
   * All the tasks that prevent a task `t` moving to a particular `start`.
   * That is: All resource-conflicting tasks, and all tasks
   * which are a dependency of `t` and start later than `start`.
   */
  def preventsMove(t: Task, tStart: Double): Set[Task] = {
    val preventingPreds = graph.predecessors(t) filter { schedule.end(_) > tStart }
    val conflicts = (schedule - t).resourceConflictingTasks(t, tStart)
    preventingPreds union conflicts.toSet
  }

}

/**
 * A `Plan` with no tasks and no dependencies.
 */
object EmptyPlan extends Plan {
  val tasks = Set[Task]()
  val dependencies = Set[(Task, Task)]()
}
