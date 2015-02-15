package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class PlanAdjusterTest extends FlatSpec with Matchers {

  "Move.samePiece" should "be false if the tasks are different" in {
    val m1 = Move(Task('t1), 2)
    val m2 = Move(Task('t2), 2)
    m1.samePiece(m2) should equal (false)
  }
  
  it should "be true for same task but different start" in {
    val m1 = Move(Task('t1), 2)
    val m2 = Move(Task('t1), 3)
    m1.samePiece(m2) should equal (true)
  }
  
  "Move.max" should "return this if other move is smaller" in {
    val m1 = Move(Task('t1), 5)
    val m2 = Move(Task('t1), 3)
    m1.max(m2) should equal (m1)
  }
  
  it should "return the other move if this move is smaller" in {
    val m1 = Move(Task('t1), 3)
    val m2 = Move(Task('t1), 5)
    m1.max(m2) should equal (m2)
  }
}