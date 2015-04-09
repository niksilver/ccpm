package org.pigsaw.ccpm

/* Copyright Nik Silver 2015.
 * 
 * This file is part of CCPM.
 *
 * CCPM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *  
 * CCPM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with CCPM.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * A project plan, which may or may not be a good one.
 */
trait Plan {
  self =>

  /**
   * Tasks in the plan
   */
  val tasks: Iterable[Task]

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
   * Create a new plan just like this one, but with the given schedule.
   */
  def withSchedule(sch: Schedule): Plan = new Plan {
    val tasks = self.tasks
    val dependencies = self.dependencies
    override lazy val schedule = sch
  }

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

    val singletons = tasks filterNot ( graph.hasNode(_) );
    val singletonChains = singletons map { Seq(_) }

    chainedGraph.paths ++ singletonChains
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
   * Is the given task on the critical chain?
   */
  def isOnCriticalChain(t: Task): Boolean = { criticalChain contains t }

  /**
   * Get the task that follows on from this path and which
   * is on the critical chain
   */
  def feedOnCriticalChain(path: Seq[Task]): Option[Task] = {
    val nextTasks = graph.successors(path.last)
    nextTasks find { isOnCriticalChain(_) }
  }

  /**
   * Get the completion buffer (aka project buffer).
   */
  lazy val completionBuffer: CompletionBuffer = {
    val id = Buffer.nextId(tasks map { _.id })
    val duration = CompletionBuffer.duration(criticalChain)
    CompletionBuffer(id, duration, criticalChain.last)
  }
  
  /**
   * Get the completion buffer as an `Option`, because
   * there won't be one if there are no tasks.
   */
  lazy val completionBufferOption: Option[CompletionBuffer] = {
    if (tasks.isEmpty) {
      None
    } else {
      Some(completionBuffer)
    }
  }

  /**
   * Get what feeder buffers are needed.
   * @return Triples which identify
   *     (i) the last task before the buffer,
   *     (ii) the task in the critical chain, and
   *     (iii) how long the buffer should ideally be.
   */
  lazy val feederBuffersNeeded: Set[(Task, Task, Double)] = {
    def penultimate(path: Seq[Task]): Task = path(path.length - 2)
    val links = pathsToCriticalChain groupBy { path => (penultimate(path), path.last) }
    
    for {
      link <- links.keySet
      paths = links(link) map { _.init }
    } yield (link._1, link._2, FeederBuffer.duration(paths))
  }
  
  /**
   * Get all the paths (possibly overlapping) that feed into
   * the critical chain. The last task of each path will be
   * a task on the critical chain.
   */
  lazy val pathsToCriticalChain: Set[Seq[Task]] = {
    // We want all paths that terminate on the critical chain,
    // but we will end up with some one-task paths which are
    // simply a task on the critical chain. So we'll need
    // to filter those out
    val paths = graph.pathsTo(criticalChain.toSet, isOnCriticalChain)
    paths filterNot ( _.length == 1 )
  }
  
  /**
   * All the dependencies, including those involving buffers.
   */
  lazy val dependenciesWithBuffers: Set[(Period, Period)] =
    (dependencies map { d => (d._1: Period, d._2: Period) }) ++
    (completionBufferOption map { b => (b.predecessor, b) }) ++
    (bufferedSchedule.feederBuffers map { b => (b.predecessor, b) })

  /**
   * All the periods from the buffered schedule, in an appropriate
   * order (if the original `tasks` were ordered).
   * So all tasks will be in the order specified,
   * each feeder buffers will come directly after its predecessor,
   * and the completion buffer will be last. 
   */
  lazy val periodsWithBuffers: Iterable[Period] = {
    val tasksAndFeeders = for {
      task <- tasks
      feeders = bufferedSchedule.feederBuffers filter ( _.predecessor == task )
      taskOrFeeder <- Seq(task) ++ feeders
    } yield taskOrFeeder
    
    tasksAndFeeders ++ completionBufferOption
  }
  
  /**
   * Get a schedule for this plan, including buffers.
   */
  lazy val bufferedSchedule: Schedule = {
    val schWithCompletionBuffer = completionBufferOption match {
      case Some(buffer) => {
        val lastTask = criticalChain.last
        val lastTaskEnd = schedule.end(lastTask)
        schedule + (buffer, lastTaskEnd)
      }
      case None => schedule
    }

    addFeederBuffers(feederBuffersNeeded, schWithCompletionBuffer)
  }

  private def addFeederBuffers(buffs: Set[(Task, Task, Double)], sch: Schedule): Schedule = {
    if (buffs.isEmpty) {
      sch
    } else {
      val (pred, succ, bufferDuration) = buffs.head
      val bufferEnd = sch.start(succ)
      val predCalculatedStart = bufferEnd - bufferDuration - pred.duration
      val predCurrentStart = sch.start(pred)
      val predIdealstart = Math.min(predCurrentStart, predCalculatedStart)

      val move = Move(pred, predIdealstart)
      val adj = new PlanAdjuster
      val adjustedPlan = adj.solve(this.withSchedule(sch), move)
      val bufferActualStart = adjustedPlan.schedule.end(pred)
      val bufferActualDuration = bufferEnd - bufferActualStart
      val bufferId = nextBufferId(adjustedPlan.schedule)
      val buffer = FeederBuffer(bufferId, bufferActualDuration, pred)
      
      val updatedSchedule = if (bufferActualDuration > 0) {
        adjustedPlan.schedule + (buffer, bufferActualStart)
      } else {
        adjustedPlan.schedule
      }

      addFeederBuffers(buffs.tail, updatedSchedule)
    }
  }
  
  // A new buffer ID for a given schedule
  private def nextBufferId(sch: Schedule): Symbol =
    Buffer.nextId(sch.periods map { _.id })

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
