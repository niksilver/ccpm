package org.pigsaw.ccpm

import scala.collection.mutable.ListBuffer

/**
 * A project plan, which may or may not be a good one.
 */
class Plan extends PlanVerbs {

  val taskList = ListBuffer[Task]()
  val dependenciesList = scala.collection.mutable.MutableList[(Task, Task)]()

  /**
   * Tasks in the plan, in the order in which they were created.
   */
  def tasks: List[Task] = scala.collection.immutable.List(taskList: _*)

  /**
   * A list of task pairs `t0 -> t1` where `t0` has to finish
   * before `t1` can start.
   */
  def dependencies: List[(Task, Task)] = scala.collection.immutable.List(dependenciesList: _*)
}

class DuplicateTaskException(msg: String) extends Exception(msg)
class UnknownTaskException(msg: String) extends Exception(msg)
class CyclicDependencyException(msg: String) extends Exception(msg)

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
  implicit def Id2DSLTask(id: Symbol) = {
    val tList = taskList filter { _.id == id }
    if (tList.length == 0)
      throw new UnknownTaskException("No such task with id " + id)
    else {
      new DSLTask(tList(0), this)
    }
  }

  object add {

    /**
     * Method for the syntax `add task "My description"`
     */
    def task(desc: String) = {
      val autoIds = taskList filter { t => Task.isAutoId(t.id) } map { _.id }
      val idNums = autoIds map { _.name.drop(1).toInt }
      val maxNum = idNums.fold(-1)(Math.max)
      val nextId = Symbol("t" + (maxNum + 1))
      val t = Task(nextId, desc)
      taskList += t
    }

    /**
     * Method for the syntax `add task 't100 as "My description"`
     */
    def task(id: Symbol): Task = {
      val t = Task(id)
      if (taskList exists (_ isAVariationOf t))
        throw new DuplicateTaskException("Found duplicate of task " + id)
      else {
        taskList += t
        t
      }
    }
  }

}

class DSLTask(t: Task, p: Plan) {

  /**
   * Method for the syntax `add task 't100 as "My description"`
   */
  def as(desc: String) = {
    val t2 = new Task(t.id, desc)
    p.taskList -= t += t2
  }

  /**
   * Method for the syntax `'t0 ~> 't1`
   */
  def ~>(id: Symbol): Task = {
    val tList = p.taskList filter { _.id == id }
    if (tList.length == 0)
      throw new UnknownTaskException("No such task with id " + id)
    
    val tLater = tList(0)
    val dependency = (t -> tLater)
    if (!Graph.remainsAcyclic(p.dependenciesList, dependency))
      throw new CyclicDependencyException(s"While adding $t ~> $tLater")
    
    p.dependenciesList += dependency
    tLater

  }
}
