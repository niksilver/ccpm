package org.pigsaw.ccpm

import scala.collection.mutable.ListBuffer

/**
 * Mutable data for a plan.
 */
class PlanContext {
  val taskList = ListBuffer[Task]()
  val resourcesList = scala.collection.mutable.MutableList[String]()
  val dependenciesList = scala.collection.mutable.MutableList[(Task, Task)]()

  def task(id: Symbol) = Task.task(taskList, id)
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

  protected val pc = new PlanContext

  implicit def Task2DSLTask(t: Task) = new DSLTask(t, pc)
  implicit def Id2DSLTask(id: Symbol) = new DSLTask(task(id), pc)

  object add {

    /**
     * Method for the syntax `add task "My description"`
     */
    def task(desc: String) = {
      val ids = pc.taskList map { _.id }
      val nextId = Task.nextId(ids)
      val t = Task(nextId, desc)
      pc.taskList += t
    }

    /**
     * Method for the syntax `add task 't100 as "My description"`
     */
    def task(id: Symbol): Task = {
      val t = Task(id)
      if (pc.taskList exists (_ isAVariationOf t))
        throw new DuplicateTaskException("Found duplicate of task " + id)
      else {
        pc.taskList += t
        t
      }
    }
  }

  object declare {

    /**
     * Method for the syntax `declare resource "Alice"`.
     */
    def resource(res: String) = {
      pc.resourcesList += res
    }
  }

}

/**
 * Conversion of a `Task` that will allow it to be managed
 * via the DSL.
 */
class DSLTask(t: Task, pc: PlanContext) {

  private def replaceTask(tOld: Task, tNew: Task) {
    pc.taskList -= tOld += tNew
  }
  /**
   * Method for the syntax `add task 't100 as "My description"`
   */
  def as(desc: String): Task = {
    val t2 = Task(t.id, desc, t.duration, t.resource)
    replaceTask(t, t2)
    t2
  }

  /**
   * Method for the syntax `'t0 ~> 't1`
   */
  def ~>(id: Symbol): Task = {
    val tLater = pc.task(id)
    val dependency = (t -> tLater)

    if (pc.dependenciesList contains dependency)
      throw new DuplicateDependencyException(s"Already got $t ~> $tLater")

    val g = new Graph(pc.dependenciesList)
    if (!g.remainsAcyclic(dependency))
      throw new CyclicDependencyException(s"While adding $t ~> $tLater")

    pc.dependenciesList += dependency
    tLater
  }

  /**
   * Method to define the duration of a task, as in
   * `add task 't0 duration 5`
   */
  def duration(dur: Double): Task = {
    val t2 = Task(t.id, t.description, dur, t.resource)
    replaceTask(t, t2)
    t2
  }

  /**
   * Method to define the resource for a task, as in
   * `add task 't0 resource "Alice"`
   */
  def resource(res: String): Task = {
    if (!pc.resourcesList.contains(res))
      throw new UnknownResourceException(s"""Resource "$res" not previously declared""")
    val t2 = Task(t.id, t.description, t.duration, Some(res))
    replaceTask(t, t2)
    t2
  }
}
