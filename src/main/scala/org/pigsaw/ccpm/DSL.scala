package org.pigsaw.ccpm

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
  implicit def Id2DSLTask(id: Symbol) = new DSLTask(task(id), this)

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

/**
 * Conversion of a `Task` that will allow it to be managed
 * via the DSL.
 */
class DSLTask(t: Task, p: Plan) {

  /**
   * Method for the syntax `add task 't100 as "My description"`
   */
  def as(desc: String) = {
    val t2 = Task(t.id, desc)
    p.taskList -= t += t2
  }

  /**
   * Method for the syntax `'t0 ~> 't1`
   */
  def ~>(id: Symbol): Task = {
    val tLater = p.task(id)
    val dependency = (t -> tLater)

    if (!Graph.remainsAcyclic(p.dependenciesList, dependency))
      throw new CyclicDependencyException(s"While adding $t ~> $tLater")

    p.dependenciesList += dependency
    tLater
  }
  
  /**
   * Method to define the duration of a task, as in
   * `add task 't0 duration 5`
   */
  def duration(dur: Double): Unit = {
    val t2 = Task(t.id, t.description, dur, t.resource)
    p.taskList -= t += t2
  }
}
