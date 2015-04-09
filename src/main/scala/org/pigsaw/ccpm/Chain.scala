package org.pigsaw.ccpm

/* Copyright Nik Silver 2015.
 * 
 * This file is part of CCPM.
 *
 * CCPM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *  
 * CCPM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with CCPM.  If not, see <http://www.gnu.org/licenses/>.
 */
    
import scala.collection.IterableLike

/**
 * A chain of tasks
 */
case class Chain(s: Seq[Task]) {
  
  /** Return the underlying sequence of tasks.
   */
  def toSeq: Seq[Task] = s
    
  /**
   * Get the length of the chain, which is the sum of
   * all the task durations.
   */
  def length: Double = s.foldLeft(0.0)( _ + _.duration )
  
  /**
   * Return the longest chain: either this or `that`
   */
  def max(that: Chain) = if (length > that.length) this else that
}
