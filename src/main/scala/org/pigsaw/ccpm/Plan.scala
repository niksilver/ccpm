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
   * A schedule for this plan.
   */
  lazy val schedule: Schedule = Schedule.make(tasks.toSet, dependencies)
  
  /**
   * Get all possible chains for this plan. This includes non-critical chains.
   */
  lazy val chains: Set[Seq[Task]] = {
    val g = new Graph(dependencies)
    val resPairs = schedule.resourceAdjacentTasks
    val newResPairs = resPairs filterNot { pair => g.hasEdge(pair) }
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
    val paths = (new Graph(dependencies)).paths
    buildSlices(paths, Set(), criticalChain.toSet)
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
    val g = new Graph(dependencies)
    val nextTasks = g.targets(path.last)
    nextTasks find { criticalChain contains _}
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
  lazy val completionBuffer: Buffer = Buffer.make('b0, criticalChain)
}

/**
 * A `Plan` with no tasks and no dependencies.
 */
object EmptyPlan extends Plan {
  val tasks = Set[Task]()
  val dependencies = Set[(Task, Task)]()
}
