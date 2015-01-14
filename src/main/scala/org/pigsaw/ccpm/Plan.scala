package org.pigsaw.ccpm

/**
 * A project plan, which may or may not be a good one.
 */
trait Plan {

  /**
   * Tasks in the plan
   */
  val tasks: Seq[Task]

  /**
   * Retrieve a task by its id.
   */
  def task(id: Symbol) = Task.task(tasks, id)

  /**
   * Resources in the plan.
   */
  lazy val resources: Seq[String] = (tasks map { _.resource }).flatten.distinct

  /**
   * A list of task pairs `t0 -> t1` where `t0` has to finish
   * before `t1` can start.
   */
  val dependencies: Seq[(Task, Task)]

  /**
   * A schedule for this plan.
   */
  lazy val schedule: Schedule = Schedule.make(tasks, dependencies)

  /**
   * Get all possible chains for this plan. This includes non-critical chains.
   */
  lazy val chains: Seq[Seq[Task]] = {
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
    if (tasks.length <= 1) {
      tasks
    } else {
      val longest = chains map { Chain(_) } reduce { _ max _ }
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
  def nonCriticalPaths: Seq[Seq[Task]] = {
    val paths = (new Graph(dependencies)).paths
    buildSlices(paths, Nil, criticalChain)
  }

  /**
   * Given some paths, return the slices found from removing
   * all the excluded tasks. Initially the excluded tasks are
   * those on the critical chain, but as more slices are
   * found so the tasks on those slices are added to the excluded
   * list.
   */
  private def buildSlices(paths: Seq[Seq[Task]], acc: Seq[Seq[Task]], excluded: Seq[Task]): Seq[Seq[Task]] = {
    paths match {
      case Nil => acc
      case path :: rest => {
        val (newAcc, newExcluded) = slicePath(path, acc, excluded)
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
  private def slicePath(path: Seq[Task], acc: Seq[Seq[Task]], excluded: Seq[Task]): (Seq[Seq[Task]], Seq[Task]) = {
    if (path.isEmpty) {
      (acc, excluded)
    } else {
      val usable = path dropWhile { excluded contains _ }
      val (slice, next) = usable span { t => !(excluded contains t) }
      val newAcc = if (slice.isEmpty) acc else slice +: acc
      slicePath(next, newAcc, slice ++: excluded)
    }
  }
}

/**
 * A `Plan` with no tasks and no dependencies.
 */
object EmptyPlan extends Plan {
  val tasks = Nil
  val dependencies = Nil
}
