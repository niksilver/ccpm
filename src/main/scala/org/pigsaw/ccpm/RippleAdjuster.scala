package org.pigsaw.ccpm

/**
 * A solver for the problem where in order to make a desired change
 * to a thing, we may first need to make some changes to some other
 * things, which may require changes to other things, and so on, rippling out.
 * It should return the best available solution, which may be less than
 * our original goal.
 * 
 * @type M  The move made on each state
 */
abstract class RippleAdjuster[M] {
  
  /**
   * Attempt a move on the state
   */
  def attempt(state: String, move: M): Result[M]
  def solve(state: String, move: M) = attempt(state, move)
}

/** Result of an attempted move.
 */
sealed class Result[+M]

/** The result of a wholly successful move. */
case class Success(res: String) extends Result

/** The result of a partially successful move. */
case class Partial(res: String) extends Result

/** A prerequisite for a (perhaps partially) successful move. */
case class Prerequisite[M](move: M) extends Result[M]

/** The result of an attempted move is impossible. */
object Impossible extends Result
