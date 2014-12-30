package org.pigsaw.ccpm

/**
 * A task in the project plan
 */
case class Task(id: Symbol, description: String)

object Task {
  
  val DefaultId = 't0
  /** A `Task` with default ID `t0`.
   */
  def apply(description: String) = new Task(DefaultId, description)
}
