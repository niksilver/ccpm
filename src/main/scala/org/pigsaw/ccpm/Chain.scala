package org.pigsaw.ccpm

/**
 * A chain of tasks
 */
case class Chain (s: Seq[Task]) {
  
  /** Return the underlying sequence of tasks.
   */
  def toSeq: Seq[Task] = s
  
  /**
   * Get the length of the chain, which is the sum of
   * all the task durations.
   */
  def length: Double = s.foldLeft(0.0)( _ + _.duration )
}