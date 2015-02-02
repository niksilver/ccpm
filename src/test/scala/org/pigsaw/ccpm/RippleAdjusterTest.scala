package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class RippleAdjusterTest extends FlatSpec with Matchers {
  // The "linear shunt" problem is this:
  // There is a string of letters and "."s. A letter represents a piece, and
  // a "." represents a space. We want to move an letter along the string
  // a given number of places rightwards. But it can only move into a space.
  // However, we can move another letter along if it's in the way.
  // No letter can move beyond the end of the string.

  /**
   * Description of the desire to move the letter at index `index`
   * a given number of `steps`.
   */
  case class LinShMove(index: Int, steps: Int)

  /**
   * A `RippleAdjuster` describing the linear shunt problem.
   */
  class LinShRippleAdjuster(board: String) extends RippleAdjuster[LinShMove](board) {
    def attempt(m: LinShMove): Result = {
      val letter = board(m.index)
      val scope = ((board drop (m.index+1)) takeWhile { _ == '.' }).length
      if (m.steps <= scope) {
        val res = board.updated(m.index, ".").updated(m.index + m.steps, letter)
        Success(res.mkString)
      } else if (scope == 0) {
        Impossible(board)
      } else {
        val res = board.updated(m.index, ".").updated(m.index + scope, letter)
        Partial(res.mkString)
      }
    }
  }

  "solve" should "solve a simple one-step problem (1)" in {
    val ra = new LinShRippleAdjuster("x.")
    val move = LinShMove(0, 1)
    ra.solve(move) should equal (Success(".x"))
  }

  it should "solve a simple one-step problem (2 - to avoid faking)" in {
    val ra = new LinShRippleAdjuster("x..")
    val move = LinShMove(0, 1)
    ra.solve(move) should equal (Success(".x."))
  }

  it should "solve a simple one-step problem with a different letter" in {
    val ra = new LinShRippleAdjuster("y..")
    val move = LinShMove(0, 1)
    ra.solve(move) should equal (Success(".y."))
  }

  it should "solve a simple one-step problem with a different kind of move" in {
    val ra = new LinShRippleAdjuster("x...")
    val move = LinShMove(0, 2)
    ra.solve(move) should equal (Success("..x."))
  }

  it should "recognise when the move is impossible" in {
    val ra = new LinShRippleAdjuster("...x")
    val move = LinShMove(3, 1)
    ra.solve(move) should equal (Impossible("...x"))
  }
  
  it should "return a partial success if necessary" in {
    val ra = new LinShRippleAdjuster("..a.")
    val move = LinShMove(2, 2)
    ra.solve(move) should equal (Partial("...a"))
  }
}
