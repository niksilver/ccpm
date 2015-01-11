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
  val resources: Seq[String]

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
   * Get all possible chains for this plan
   */
  lazy val chains: Seq[Seq[Task]] = {
    val g = new Graph(dependencies)
    g.paths
  }
  
}
