package org.pigsaw.ccpm

/**
 * A task in the project plan
 */
case class Task(id: Symbol, description: String, duration: Double, resource: Option[String]) {

  /**
   * One task is a variation of another if and only if
   * the ids are the same.
   */
  def isAVariationOf(t: Task) = (id == t.id)

  lazy val halfDuration: Double = duration / 2
}

object Task {

  val DefaultId = 't0
  val DefaultDescription = "Anonymous task"
  val DefaultDuration = 0.0

  /**
   * A `Task` with just a description defined. Everything else is
   *  the default
   */
  def apply(description: String) = new Task(DefaultId, description, 0, None)

  /**
   * A `Task` with just the `id` defined by the user.
   */
  def apply(id: Symbol) = new Task(id, DefaultDescription, 0, None)

  /**
   * A `Task` with just the `id` and `description` set by the user.
   */
  def apply(id: Symbol, description: String) = new Task(id, description, 0, None)

  /**
   * A `Task` with just the `id` and `duration` set by the user.
   */
  def apply(id: Symbol, duration: Double) = new Task(id, DefaultDescription, duration, None)

  /**
   * Is a given symbol of the format for an auto id?
   * True when it's of the format `t` followed by one or more digits.
   */
  def isAutoId(id: Symbol): Boolean = {
    val name = id.name
    name(0) == 't' &&
      name.length >= 2 &&
      (name.drop(1) forall { _.isDigit })
  }

  /**
   * Given a list of ids, generate the next appropriate one.
   */
  def nextId(ids: Seq[Symbol]): Symbol = {
    val autoIds = ids filter { id => Task.isAutoId(id) }
    val idNums = autoIds map { _.name.drop(1).toInt }
    val maxNum = idNums.fold(-1)(Math.max)
    Symbol("t" + (maxNum + 1))
  }
  
  /**
   * Given some tasks, get one by its id, or throw an
   * `UnknownTaskException`.
   */
  def task(ts: Seq[Task], id: Symbol): Task = {
    (ts find { _.id == id}) match {
      case Some(t) => t
      case None => throw new UnknownTaskException("No such task with id " + id)
    }
  }

}
