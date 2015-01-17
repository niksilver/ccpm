package org.pigsaw.ccpm

trait Period {
  val id: Symbol
  val duration: Double
}

/**
 * The capability to work out ids automatically. 
 */
class AutoIding(private val prefix: String) {

  private val length = prefix.length
  
  private def split(name: String): (String, String) = name.splitAt(length)
  private def splitName(id: Symbol): (String, String) = split(id.name)

  /**
   * Is a given symbol of the format for an auto id?
   * True when it's of the format `t` followed by one or more digits.
   */
  def isAutoId(id: Symbol): Boolean = {
    val (half1, half2) = splitName(id)
    half1 == prefix &&
      half2.length >= 1 &&
      (half2 forall { _.isDigit })
  }

  /**
   * Given a list of ids, generate the next appropriate one.
   */
  def nextId(ids: Iterable[Symbol]): Symbol = {
    val autoIds = ids filter { id => isAutoId(id) }
    val idNums = autoIds map { splitName(_)._2.toInt }
    val maxNum = idNums.fold(-1)(Math.max)
    Symbol(prefix + (maxNum + 1))
  }

}

/**
 * A task in the project plan. The `duration` is the duration of the
 * task as we will work with it in the plan. So if we're doing CCPM
 * then the duration should be set as the 50%-likelihood completion time.
 * If you want to capture something else, like the 95%-likelihood completion
 * time as well, then you should consider subclassing this.
 */
case class Task(id: Symbol, description: String, duration: Double, resource: Option[String]) extends Period {

  /**
   * One task is a variation of another if and only if
   * the ids are the same.
   */
  def isAVariationOf(t: Task) = (id == t.id)
  
  /**
   * True if and only if this and `t2` both have `Some` resource
   * and they are they are equal.
   */
  def sameResource(t2: Task): Boolean = { resource.nonEmpty && resource == t2.resource }

  override def toString() = id.toString
}

object Task extends AutoIding("t") {

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
   * Given some tasks, get one by its id, or throw an
   * `UnknownTaskException`.
   */
  def task(ts: scala.collection.Set[Task], id: Symbol): Task = {
    (ts find { _.id == id}) match {
      case Some(t) => t
      case None => throw new UnknownTaskException("No such task with id " + id)
    }
  }

}

case class Buffer(id: Symbol, duration: Double) extends Period

object Buffer extends AutoIding("b")
