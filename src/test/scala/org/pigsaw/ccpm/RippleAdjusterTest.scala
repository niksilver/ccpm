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
  class LinShRippleAdjuster extends RippleAdjuster[String,LinShMove] {
    
    def attempt(board: String, m: LinShMove): Seq[Attempt[LinShMove]] = {
      val maxIndex = board.size - 1
      val availableSteps = ((board drop (m.index+1)) takeWhile { _ == '.' }).length
      val availableIndex = m.index + availableSteps
      if (m.steps <= availableSteps) {
        // We can comfortably make the move
        Seq(Actual(LinShMove(m.index, m.steps)))
      } else if (m.index == maxIndex) {
        // We cannot move
        Seq()
      } else if (availableIndex == maxIndex) {
        // We can move, but only to the end of the board
        Seq(Actual(LinShMove(m.index, availableSteps)))
      } else {
        // We're stopped from moving all the way by another piece
        // so we have a prerequisite of moving that the remaining number of steps
        val prereqLetterIdx = availableIndex + 1
        val prereqSteps = Math.min(maxIndex - prereqLetterIdx, m.steps - availableSteps)
        Seq(Prerequisite(LinShMove(prereqLetterIdx, prereqSteps)))
      }
    }
    
    def make(board: String, m: LinShMove): String = {
      val letter = board(m.index)
      val availableSteps = ((board drop (m.index+1)) takeWhile { _ == '.' }).length
      val actualSteps = Math.min(m.steps, availableSteps)
      val result = board.updated(m.index, ".").updated(m.index + actualSteps, letter)
      result.mkString
    }
    
  }

  "solve" should "solve a simple one-step problem (1)" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(0, 1)
    ra.solve("x.", move) should equal (".x")
  }

  it should "solve a simple one-step problem (2 - to avoid faking)" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(0, 1)
    ra.solve("x..", move) should equal (".x.")
  }

  it should "solve a simple one-step problem with a different letter" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(0, 1)
    ra.solve("y..", move) should equal (".y.")
  }

  it should "solve a simple one-step problem with a different kind of move" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(0, 2)
    ra.solve("x...", move) should equal ("..x.")
  }

  it should "complete even if move is impossible" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(3, 1)
    ra.solve("...x", move) should equal ("...x")
  }
  
  it should "ripple prerequisites once" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(2, 2)
    ra.solve("..ab..", move) should equal ("....ab")
  }
  
  it should "ripple prerequisites twice" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(2, 1)
    ra.solve("..abc.", move) should equal ("...abc")
  }
  
  it should "ripple prerequisites many times" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(2, 2)
    ra.solve("..abc.d..", move) should equal ("....abcd.")
  }
  
  it should "solve with a partial solution, with rippling, if necessary" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(2, 4)
    ra.solve("..abc.d..", move) should equal (".....abcd")
  }
  
  it should "solve with a partial solution, even if one end move is impossible" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(2, 3)
    ra.solve("..abc.d", move) should equal ("...abcd")
  }
  
  it should "return the same state if any rippling of moves is impossible" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(3, 2)
    ra.solve("..abcde", move) should equal ("..abcde")
  }
  
  "LinShRippleAdjuster.attempt" should "return a prerequisite if necessary (1)" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(1, 1)
    ra.attempt(".ab.", move) should equal (Seq(Prerequisite(LinShMove(2, 1))))
  }
  
  it should "return a prerequisite if necessary (2 - to avoid faking)" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(2, 1)
    ra.attempt("..ab..", move) should equal (Seq(Prerequisite(LinShMove(3, 1))))
  }
  
  it should "return a prerequisite if necessary (3 - to avoid faking again)" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(2, 2)
    ra.attempt("..ab..", move) should equal (Seq(Prerequisite(LinShMove(3, 2))))
  }
  
  it should "require later pieces move forward if current piece can only make it part-way" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(1, 2)
    ra.attempt(".a.b.", move) should equal (Seq(Prerequisite(LinShMove(3, 1))))
  }
  
  it should "allow the last piece to move only part-way if necessary" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(1, 3)
    ra.attempt(".a.b.", move) should equal (Seq(Prerequisite(LinShMove(3, 1))))
  }
  
  it should "not require a letter to exist beyond the end of the board" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(1, 3)
    ra.attempt(".a.", move) should equal (Seq(Actual(LinShMove(1,1))))
  }
  
  it should "return an empty sequence if no move is possible" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(2, 1)
    ra.attempt("..a", move) should equal (Seq())
  }
}
