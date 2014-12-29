package org.pigsaw.ccpm

/**
 * A project plan, which may or may not be a good one.
 */
class Plan(details: => Unit) {

}

object Plan {
  def apply(details: => Unit) = new Plan(details)
}