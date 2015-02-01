package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class RippleAdjusterTest extends FlatSpec with Matchers {
  // The "linear shunt" problem is this:
  // There is a string of "x"s and "."s. An "x" represents a piece, and
  // a "." represents a space. We want to move an "x" along the string
  // a given number of places rightwards. But it can only move into a space.
  // However, we can move another "x" along if it's in the way.
  // No "x" can move beyond the end of the string.
  
  /**
   * Description of the desire to move the "x" at index `index`
   * a given number of `steps`.
   */
  case class LinShMove(index: Int, steps: Int)
  
  /**
   * A `RippleAdjuster` describing the linear shunt problem.
   */
  class LinShRippleAdjuster(board: String) extends RippleAdjuster[LinShMove](board) {
    def attempt(m: LinShMove) = {
      board.updated(m.index, ".").updated(m.index + 1, "x").mkString
    }
  }
  
  "solve" should "solve a simple one-step problem (1)" in {
    val ra = new LinShRippleAdjuster("x.")
    val move = LinShMove(0, 1)
    ra.solve(move) should equal (".x")
  }
  
  it should "solve a simple one-step problem (2 - to avoid faking)" in {
    val ra = new LinShRippleAdjuster("x..")
    val move = LinShMove(0, 1)
    ra.solve(move) should equal (".x.")
  }
}