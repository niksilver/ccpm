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
  class LinShRippleAdjuster extends RippleAdjuster[LinShMove] {
    def attempt(board: String, m: LinShMove) = {
      val letter = board(m.index)
      val scope = ((board drop (m.index+1)) takeWhile { _ == '.' }).length
      if (m.steps <= scope) {
        val res = board.updated(m.index, ".").updated(m.index + m.steps, letter)
        Completed(res.mkString)
      } else if (m.index == board.length - 1) {
        Impossible
      } else if (scope == 0) {
        Prerequisite(LinShMove(m.index+1, m.steps))
      } else {
        val res = board.updated(m.index, ".").updated(m.index + scope, letter)
        Completed(res.mkString)
      }
    }
  }

  "solve" should "solve a simple one-step problem (1)" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(0, 1)
    ra.solve("x.", move) should equal (Completed(".x"))
  }

  it should "solve a simple one-step problem (2 - to avoid faking)" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(0, 1)
    ra.solve("x..", move) should equal (Completed(".x."))
  }

  it should "solve a simple one-step problem with a different letter" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(0, 1)
    ra.solve("y..", move) should equal (Completed(".y."))
  }

  it should "solve a simple one-step problem with a different kind of move" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(0, 2)
    ra.solve("x...", move) should equal (Completed("..x."))
  }

  it should "recognise when the move is impossible" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(3, 1)
    ra.solve("...x", move) should equal (Impossible)
  }
  
  it should "ripple prerequisites once" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(2, 2)
    ra.solve("..ab..", move) should equal (Completed("....ab"))
  }
  
  it should "ripple prerequisites twice" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(2, 1)
    ra.solve("..abc.", move) should equal (Completed("...abc"))
  }
  
  "LinShRippleAdjuster.attempt" should "return a prerequisite if necessary (1)" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(1, 1)
    ra.attempt(".ab.", move) should equal (Prerequisite(LinShMove(2, 1)))
  }
  
  it should "return a prerequisite if necessary (2 - to avoid faking)" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(2, 1)
    ra.attempt("..ab..", move) should equal (Prerequisite(LinShMove(3, 1)))
  }
  
  it should "return a prerequisite if necessary (3 - to avoid faking again)" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(2, 2)
    ra.attempt("..ab..", move) should equal (Prerequisite(LinShMove(3, 2)))
  }
}
