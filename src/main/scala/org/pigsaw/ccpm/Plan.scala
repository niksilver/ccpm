package org.pigsaw.ccpm

/**
 * A project plan, which may or may not be a good one.
 */
class Plan(details: => Unit) {
  
  def tasks: List[Task] = List(Task("My task 1"), Task("My task 2"))
}

object Plan {
  def apply(details: => Unit) = new Plan(details)
}