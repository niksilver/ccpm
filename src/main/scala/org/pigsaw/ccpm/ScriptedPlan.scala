package org.pigsaw.ccpm

/**
 * A project plan described with the DSL.
 */
class ScriptedPlan extends PlanVerbs with Plan {

  /**
   * Tasks in the plan, in the order in which they were created.
   */
  lazy val tasks: Seq[Task] = scala.collection.immutable.List(pc.tasks: _*)
  
  /**
   * Resources in the plan, in the order in which they were declared
   */
  override lazy val resources: Set[String] = scala.collection.immutable.Set(pc.resources: _*)

  /**
   * A list of task pairs `t0 -> t1` where `t0` has to finish
   * before `t1` can start.
   */
  lazy val dependencies: Set[(Task, Task)] = scala.collection.immutable.Set(pc.dependencies: _*)
}
