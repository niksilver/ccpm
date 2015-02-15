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
trait RippleAdjuster[S, M <: Move[M]] {

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
  def solve(state: S, move: M): S = {
    // Work out our desired moves, and then the actual
    // moves that happen when we really make them
    val moves = desiredMoves(state, move)
    println(s"(1) moves = $moves")
    val (_, actualMoves) = makeMoves(state, moves, Nil)
    println(s"(2) actualMoves = $actualMoves")
    // Now we can find what actually happened to our
    // original move. So let's use that to find the
    // minimal sequence of moves to achieve that,
    // and then make those moves.
    val moves2 = desiredMoves(state, actualMoves.head)
    println(s"(3) moves2 = $moves2")
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
        //val uniqueNewMoves = newMoves filter { !acc.contains(_) }
        val combinedAcc = combine(newMoves, acc)
        val prereqMoves = atts filter { _.isPrerequisite } map { _.get }
        //desiredMoves0(state, prereqMoves, uniqueNewMoves ++: acc)
        desiredMoves0(state, prereqMoves, combinedAcc)
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
        println(s"    make move $m yields actual $actualMove")
        makeMoves(state2, rest, actualMove :: actualMoves)
      }
    }

  /**
   * Prepend the moves `ms1` onto the start of `ms2`, but
   * if there are any pieces moved in `ms2` and `ms1` then
   * we only include the max of the two moves, and it's
   * inserted where the later `ms1` move appears.
   */
  def combine(ms1: List[M], ms2: List[M]): List[M] = {
    def getRepeat(m1: M): Option[M] = (ms2 filter { _.samePiece(m1) }).headOption
    val uniques = ms2 filterNot { m2 => ms1 exists { _.samePiece(m2) } }
    val ms1Deduped = dedupe(ms1)
    val ms1WithMaxMoves = ms1Deduped map { m1 =>
      getRepeat(m1) match {
        case None => m1
        case Some(m2) => m1.max(m2)
      }
    }
    println(s"   $ms1 + $ms2 combines to give")
    println(s"   " + (ms1WithMaxMoves ++: uniques))
    ms1WithMaxMoves ++: uniques
  }

  private def dedupe(ms: List[M]): List[M] = {
    dedupe0(ms, Nil)
  }

  private def dedupe0(ms: List[M], acc: List[M]): List[M] =
    ms match {
      case Nil => acc.reverse
      case m :: rest => {
    	val mDeduped = if (acc contains m) Nil else List(m)
    	dedupe0(rest, mDeduped ++: acc)
      }
    }
}

/**
 * A move on a piece. `M` is is the type of the extending class.
 */
trait Move[M <: Move[M]] {
  /**
   * Is `m2` a move on the same piece?
   */
  def samePiece(m2: M): Boolean

  /**
   * Return the greater move
   */
  def max(m2: M): M
}

/**
 * Result of an attempt to make a move.
 */
sealed trait Attempt[+Move] {
  def get: Move
  def isActual: Boolean
  def isPrerequisite: Boolean
}

/** An actual move to be made. */
case class Actual[+Move](move: Move) extends Attempt[Move] {
  def get: Move = move
  def isActual: Boolean = true
  def isPrerequisite: Boolean = false
}

/** A prerequisite for another move. */
case class Prerequisite[+Move](move: Move) extends Attempt[Move] {
  def get: Move = move
  def isActual: Boolean = false
  def isPrerequisite: Boolean = true
}
