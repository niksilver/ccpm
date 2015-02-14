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
   * return the resulting state, and the actual move made.
   * Note the we may not be able
   * to make the `move` to its full extent, but we will do
   * as much as we can (if anything).
   */
  def make(state: S, move: M): (S, M)

  /**
   * Make the desired `move` from the given `state`, ensuring any
   * necessary prerequisites moves are made to achieve this.
   */
  def solve(state: S, move: M): S =  {
    // Work out our desired moves, and then the actual
    // moves that happen when we really make them
    val moves = desiredMoves(state, move)
    val (_, actualMoves) = makeMoves(state, moves, Nil)
    // Now we can find what actually happened to our
    // original move. So let's use that to find the
    // minimal sequence of moves to achieve that,
    // and then make those moves.
    val moves2 = desiredMoves(state, actualMoves.head)
    val (state2, _) = makeMoves(state, moves2, Nil)
    state2
  }

//  private def solve0(state: S, moves: List[M]): S =
//    moves match {
//      case Nil => state
//      case m :: rest => {
//        val atts = attempt(state, m)
//        val attmoves = atts map { _.get }
//        if (atts exists { _.isPrerequisite }) {
//          solve0(state, attmoves ++: m +: rest)
//        } else {
//          makeMoves(state, attmoves ++: rest)
//        }
//      }
//    }
  
  /**
   * Work out what moves we should be making in order to achieve
   * the desired `move` from the given `state`. The list of moves
   * returned is in order that they should be made. They are
   * the moves we would need to make, although they may not be
   * fully possible.
   */
  def desiredMoves(state: S, move: M): List[M] = {
    desiredMoves0(state, List(move), List(move))
  }

  private def desiredMoves0(state: S, movesToAttempt: List[M], acc: List[M]): List[M] = {
    movesToAttempt match {
      case Nil => acc
      case _ => {
        val atts = movesToAttempt flatMap { attempt(state, _) }
        val newMoves = atts map { _.get }
        val uniqueNewMoves = newMoves filter { !acc.contains(_) }
        val prereqMoves = atts filter { _.isPrerequisite } map { _.get }
        desiredMoves0(state, prereqMoves, uniqueNewMoves ++: acc)
      }
    }
  }

  // Make the given `moves`, as best as possible.
  // Return the resulting state and the actual moves made
  // (most recent first).
  private def makeMoves(state: S, moves: List[M], actualMoves: List[M]): (S, List[M]) =
    moves match {
      case Nil => (state, actualMoves)
      case m :: rest => {
        val (state2, actualMove) = make(state, m)
        makeMoves(state2, rest, actualMove :: actualMoves)
      }
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
