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
  def task(ts: Iterable[Task], id: Symbol): Task = {
    (ts find { _.id == id }) match {
      case Some(t) => t
      case None => throw new UnknownTaskException("No such task with id " + id)
    }
  }

}

/**
 * A buffer in a critical chain plan.
 * Could be a completion buffer (aka project buffer) or a feeder buffer.
 */
sealed abstract class Buffer extends Period {
  val id: Symbol
  val duration: Double
  val predecessor: Task
}

object Buffer extends AutoIding("b")

case class CompletionBuffer(id: Symbol, duration: Double, predecessor: Task) extends Buffer

object CompletionBuffer {
  /**
   * Duration of a completion buffer, given the critical chain.
   */
  def duration(chain: Seq[Task]): Double = Chain(chain).length * 0.5
}

case class FeederBuffer(id: Symbol, duration: Double, predecessor: Task) extends Buffer

object FeederBuffer {

  /**
   * What we need to multiply a path length by to get the duration
   * of its feeder buffer.
   */
  val Factor: Double = 0.5

  /**
   * Given a number of paths that feed into (but which exclude)
   * the critical chain, work out the buffer duration needed.
   * According to
   * [[http://www.pmknowledgecenter.com/node/273]]
   * this is the 'cut and paste' method.
   */
  def duration(paths: Set[Seq[Task]]): Double = {
    if (paths.size == 0) {
      0
    } else {
      val maxPathLength = (paths map { Chain(_).length }).max
      maxPathLength * Factor
    }
  }

}
