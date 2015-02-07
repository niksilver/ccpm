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
    def attempt(board: String, m: LinShMove) = {
      val maxIndex = board.size - 1
      val letter = board(m.index)
      val availableSteps = ((board drop (m.index+1)) takeWhile { _ == '.' }).length
      val availableIndex = m.index + availableSteps
      if (m.steps <= availableSteps) {
        val board2 = update(board, m.index, m.steps)
        Completed(board2)
      } else if (availableIndex == maxIndex) {
        val board2 = update(board, m.index, availableSteps)
        Completed(board2)
      } else {
        // 0 < availableSteps < m.steps
        val prereqLetterIdx = availableIndex + 1
        val prereqSteps = Math.min(maxIndex - prereqLetterIdx, m.steps - availableSteps)
        Prerequisite(LinShMove(prereqLetterIdx, prereqSteps))
      }
    }
    
    def make(board: String, m: LinShMove): String = {
      val letter = board(m.index)
      val availableSteps = ((board drop (m.index+1)) takeWhile { _ == '.' }).length
      val actualSteps = Math.min(m.steps, availableSteps)
      val result = board.updated(m.index, ".").updated(m.index + actualSteps, letter)
      result.mkString
    }
    
    def update(board: String, index: Int, steps: Int): String = {
      val letter = board(index)
      val result = board.updated(index, ".").updated(index + steps, letter)
      result.mkString
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

  it should "complete even if move is impossible" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(3, 1)
    ra.solve("...x", move) should equal (Completed("...x"))
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
  
  it should "ripple prerequisites many times" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(2, 2)
    ra.solve("..abc.d..", move) should equal (Completed("....abcd."))
  }
  
  it should "solve with a partial solution, with rippling, if necessary" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(2, 3)
    ra.solve("..abc.d..", move) should equal (Completed(".....abcd"))
  }
  
  it should "solve with a partial solution, even if one end move is impossible" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(2, 3)
    ra.solve("..abc.d", move) should equal (Completed("...abcd"))
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
  
  it should "require later pieces move forward if current piece can only make it part-way" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(1, 2)
    ra.attempt(".a.b.", move) should equal (Prerequisite(LinShMove(3, 1)))
  }
  
  it should "allow the last piece to move only part-way if necessary" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(1, 3)
    ra.attempt(".a.b.", move) should equal (Prerequisite(LinShMove(3, 1)))
  }
  
  it should "not require a letter to exist beyond the end of the board" in {
    val ra = new LinShRippleAdjuster
    val move = LinShMove(1, 3)
    ra.attempt(".a.", move) should equal (Completed("..a"))
  }
}
