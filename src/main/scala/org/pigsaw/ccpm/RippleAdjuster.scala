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
abstract class RippleAdjuster[M](state: String) {
  
  /**
   * Attempt a move on the state
   */
  def attempt(move: M): String
  def solve(move: M) = attempt(move)
}

/** Result of an attempted move.
 */
sealed class Result
case class Success(res: String) extends Result
case class Impossible(res: String) extends Result
