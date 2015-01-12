package org.pigsaw.ccpm

/**
 * A chain of tasks
 */
case class Chain (s: Seq[Task]) {
  
  /** Return the underlying sequence of tasks.
   */
  def toSeq: Seq[Task] = s
}