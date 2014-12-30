package org.pigsaw.ccpm

import scala.collection.mutable.ListBuffer

/**
 * A project plan, which may or may not be a good one.
 */
class Plan extends PlanVerbs {
  
  val taskList = ListBuffer[Task]()
  def tasks: List[Task] = scala.collection.immutable.List(taskList: _*)

}

/**
 * The DSL for a `Plan` object. Example syntax:
 * {{{
 * new Plan {
 *   add task "Set up database"
 *   add task 't100 as "Populate database"
 * }
 * }}}
 */
trait PlanVerbs {
  this: Plan =>
  
  implicit def Task2DSLTask(t: Task) = new DSLTask(t, this)
  
  object add {
    
    /**
     * Method for the syntax `add task "My description"`
     */
    def task(desc: String) = {
      val t = Task(desc)
      taskList += t
    }
    
    /**
     * Method for the syntax `add task 't100 as "My description"`
     */
    def task(id: Symbol): Task = {
      val t = Task(id)
      taskList += t
      t
    }
  }
  
}

class DSLTask(t: Task, p: Plan) {
  def as(desc: String) = {
    val t2 = new Task(t.id, desc)
    p.taskList -= t += t2
  }
}
