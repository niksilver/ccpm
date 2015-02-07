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
   * Attempt a move on the state. It should return one of three things:
   * `Completed(s2)` if the move can be made (as best as possible) resulting in the new state;
   * `Prerequisite(m2)` if another move `m2` is required before we can make the desired `move` as best as possible;
   * `Impossible` if we cannot even potentially make this move.
   */
  def attempt(state: String, move: M): Result[M]
  
  /**
   * Make the desired `move` from the given `state`, ensuring any
   * necessary prerequisites moves are made to achieve this. 
   */
  def solve(state: String, move: M): Result[M] = solve0(state, List(move))
  
  private def solve0(state: String, moves: List[M]): Result[M] =
    moves match {
    case Nil => Completed(state)
    case m :: rest => attempt(state, m) match {
      case Impossible => Impossible
      case Completed(s2) => solve0(s2, rest)
      case Prerequisite(m2) => solve0(state, m2 :: m :: rest)
    }
  }
}

/**
 * Result of an attempted move.
 */
sealed abstract class Result[+M]

/** The result of a completed move. */
case class Completed(res: String) extends Result

/** A prerequisite for a move. */
case class Prerequisite[M](move: M) extends Result[M]

/** The result of an attempted move is impossible. */
object Impossible extends Result
