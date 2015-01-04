package org.pigsaw.ccpm

/**
 * A project plan, which may or may not be a good one.
 */
class Plan extends PlanVerbs {

  /**
   * Tasks in the plan, in the order in which they were created.
   */
  lazy val tasks: Seq[Task] = scala.collection.immutable.List(pc.tasks: _*)

  /**
   * Retrieve a task by its id.
   */
  def task(id: Symbol) = Task.task(tasks, id)
  
  /**
   * Resources in the plan, in the order in which they were declared
   */
  lazy val resources: Seq[String] = scala.collection.immutable.List(pc.resources: _*)

  /**
   * A list of task pairs `t0 -> t1` where `t0` has to finish
   * before `t1` can start.
   */
  lazy val dependencies: Seq[(Task, Task)] = scala.collection.immutable.List(pc.dependencies: _*)
}
