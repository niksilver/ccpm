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
  
  object declare {
    
    /**
     * Method for the syntax `declare resource "Alice"`.
     */
    def resource(res: String) = {
      resourcesList += res
    }
  }

}

/**
 * Conversion of a `Task` that will allow it to be managed
 * via the DSL.
 */
class DSLTask(t: Task, p: Plan) {

  private def replaceTask(tOld: Task, tNew: Task) {
    p.taskList -= tOld += tNew
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
    val tLater = p.task(id)
    val dependency = (t -> tLater)
    
    if (p.dependenciesList contains dependency)
      throw new DuplicateDependencyException(s"Already got $t ~> $tLater")

    val g = new Graph(p.dependenciesList)
    if (!g.remainsAcyclic(dependency))
      throw new CyclicDependencyException(s"While adding $t ~> $tLater")

    p.dependenciesList += dependency
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
    if (!p.resourcesList.contains(res))
      throw new UnknownResourceException(s"""Resource "$res" not previously declared""")
    val t2 = Task(t.id, t.description, t.duration, Some(res))
    replaceTask(t, t2)
    t2
  }
}
