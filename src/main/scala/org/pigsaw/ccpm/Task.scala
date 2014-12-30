package org.pigsaw.ccpm

/**
 * A task in the project plan
 */
case class Task(id: Symbol, description: String)

object Task {
  
  val DefaultId = 't0
  val DefaultDescription = "Anonymous task"
  
  /** A `Task` with default ID `t0`.
   */
  def apply(description: String) = new Task(DefaultId, description)
  
  /** A `Task` with default description.
   */
  def apply(id: Symbol) = new Task(id, DefaultDescription)
}
