package org.pigsaw.ccpm

import scala.collection.mutable.MutableList

/**
 * A project plan, which may or may not be a good one.
 */
class Plan extends PlanVerbs {
  
  val taskList = MutableList[Task]()
  def tasks: List[Task] = scala.collection.immutable.List(taskList: _*)

}

trait PlanVerbs {
  this: Plan =>
    
  object add {
    def task(desc: String) = {
      val t = Task(desc)
      taskList += t
    }
  }
  
}
