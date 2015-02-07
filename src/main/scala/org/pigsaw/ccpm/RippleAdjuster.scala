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
abstract class RippleAdjuster[S,M] {

  /**
   * Attempt a move on the state. It should return one of two things:
   * `Completed(s2)` if the move can be made (as best as possible) resulting in the a state `s2`;
   * `Prerequisite(m2)` if another move `m2` is required before we can make the desired `move` as best as possible;
   */
  def attempt(state: S, move: M): Result[S,M]
  
  /**
   * From the given `state` make a `move` as best as possible and
   * return the resulting state.
   */
  def make(state: S, move: M): S
  
  /**
   * Make the desired `move` from the given `state`, ensuring any
   * necessary prerequisites moves are made to achieve this. 
   */
  def solve(state: S, move: M): Result[S,M] = solve0(state, List(move))
  
  private def solve0(state: S, moves: List[M]): Result[S,M] =
    moves match {
    case Nil => Completed(state)
    case m :: rest => attempt(state, m) match {
      case Completed(s2) => makeMoves(s2, rest)
      case Prerequisite(m2) => solve0(state, m2 :: m :: rest)
    }
  }
  
  private def makeMoves(state: S, moves: List[M]): Completed[S,M] =
    moves match {
    case Nil => Completed(state)
    case m :: rest => makeMoves(make(state, m), rest)
  }
}

/**
 * Result of an attempted move.
 */
sealed abstract class Result[+S,+M]

/** The result of a completed move. */
case class Completed[+S,+M](result: S) extends Result[S,M]

/** A prerequisite for a move. */
case class Prerequisite[+S,+M](move: M) extends Result[S,M]
