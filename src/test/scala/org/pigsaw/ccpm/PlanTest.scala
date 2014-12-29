package org.pigsaw.ccpm

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class PlanTest extends FlatSpec with Matchers {
  
  "A Plan" should "be constructable with an empty block" in {
    Plan {}
  }
  
  it should "be able to accept a Task" in {
    val p = Plan {
      Task("My task 1")
    }
  }
  
  it should "be able to return the list of tasks specified" in {
    val p1 = Plan {
      Task("My task 1")
      Task("My task 2")
    }
    (p1.tasks)(0) should equal (Task("My task 1"))
    (p1.tasks)(1) should equal (Task("My task 2"))
  }
}