package org.pigsaw.ccpm

/**
 * A task in the project plan
 */
case class Task(id: Symbol, description: String) {
  
  /**
   * One task is a variation of another if and only if
   * the ids are the same.
   */
  def isAVariationOf(t: Task) = (id == t.id)
}

object Task {
  
  val DefaultId = 't0
  val DefaultDescription = "Anonymous task"
  
  /** A `Task` with default ID `t0`.
   */
  def apply(description: String) = new Task(DefaultId, description)
  
  /** A `Task` with default description.
   */
  def apply(id: Symbol) = new Task(id, DefaultDescription)
  
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
