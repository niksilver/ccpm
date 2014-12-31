package org.pigsaw.ccpm

/**
 * A task in the project plan
 */
case class Task(id: Symbol, description: String, duration: Double) {
  
  /**
   * One task is a variation of another if and only if
   * the ids are the same.
   */
  def isAVariationOf(t: Task) = (id == t.id)
  
  lazy val halfDuration: Double = duration/2
}

object Task {
  
  val DefaultId = 't0
  val DefaultDescription = "Anonymous task"
  val DefaultDuration = 0.0
  
  /** A `Task` with default ID.
   */
  def apply(description: String) = new Task(DefaultId, description, 0)
  
  /** A `Task` with just the `id` set.
   */
  def apply(id: Symbol) = new Task(id, DefaultDescription, 0)
  
  /** A `Task` with just the `id` and `description` set.
   */
  def apply(id: Symbol, description: String) = new Task(id, description, 0)
  
  /** A `Task` with just the `id` and `duration` set.
   */
  def apply(id: Symbol, duration: Double) = new Task(id, DefaultDescription, duration)
  
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
}
