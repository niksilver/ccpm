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
trait RippleAdjuster[S, M] {

  /**
   * Attempt a move on the state. It should return sequence of zero or
   * more things to achieve this. Each item in the sequence is one of
   * two things:
   * `Actual(m)` an actual move `m` that should be made;
   * `Prerequisite(m2)` if another move `m2` is required before we can make the desired `move` as best as possible;
   */
  def attempt(state: S, move: M): Seq[Attempt[M]]

  /**
   * From the given `state` make a `move` as best as possible and
   * return the resulting state. Note the we may not be able
   * to make the `move` to its full extent, but we will do
   * as much as we can (if anything).
   */
  def make(state: S, move: M): S

  /**
   * Make the desired `move` from the given `state`, ensuring any
   * necessary prerequisites moves are made to achieve this.
   */
  def solve(state: S, move: M): S = solve0(state, List(move))

  private def solve0(state: S, moves: List[M]): S =
    moves match {
      case Nil => state
      case m :: rest => {
        val atts = attempt(state, m)
        val moves = atts map { _.get }
        if (atts exists { _.isPrerequisite }) {
          solve0(state, moves ++: m +: rest)
        } else {
          makeMoves(state, moves ++: rest)
        }
      }
    }

  private def makeMoves(state: S, moves: List[M]): S =
    moves match {
      case Nil => state
      case m :: rest => makeMoves(make(state, m), rest)
    }
}

/**
 * Result of an attempt to make a move.
 */
sealed trait Attempt[+M] {
  def get: M
  def isActual: Boolean
  def isPrerequisite: Boolean
}

/** An actual move to be made. */
case class Actual[+M](move: M) extends Attempt[M] {
  def get: M = move
  def isActual: Boolean = true
  def isPrerequisite: Boolean = false
}

/** A prerequisite for another move. */
case class Prerequisite[+M](move: M) extends Attempt[M] {
  def get: M = move
  def isActual: Boolean = false
  def isPrerequisite: Boolean = true
}
