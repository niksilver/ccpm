package org.pigsaw.ccpm

/**
 * A project plan, which may or may not be a good one.
 */
class Plan extends PlanVerbs {

  /**
   * Tasks in the plan, in the order in which they were created.
   */
  def tasks: List[Task] = scala.collection.immutable.List(taskList: _*)

  def task(id: Symbol) = {
    val tList = taskList filter { _.id == id }
    if (tList.length == 0)
      throw new UnknownTaskException("No such task with id " + id)
    else
      tList(0)
  }
  
  /**
   * Resources in the plan, in the order in which they were declared
   */
  def resources: Seq[String] = scala.collection.immutable.List(resourcesList: _*)

  /**
   * A list of task pairs `t0 -> t1` where `t0` has to finish
   * before `t1` can start.
   */
  def dependencies: List[(Task, Task)] = scala.collection.immutable.List(dependenciesList: _*)
}
