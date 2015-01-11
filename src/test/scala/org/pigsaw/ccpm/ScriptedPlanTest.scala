package org.pigsaw.ccpm

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class ScriptedPlanTest extends FlatSpec with Matchers {

  "A ScriptedPlan" should "be constructable with an empty block" in {
    new ScriptedPlan {}
  }

  it should "be able to accept a Task" in {
    val p = new ScriptedPlan {
      Task("My task 1")
    }
  }

  "tasks" should "be able to return the list of tasks specified (1)" in {
    val p1 = new ScriptedPlan {
      add task "My task"
    }
    println("Starting assertions")
    (p1.tasks)(0) should equal(Task("My task"))
    p1.tasks.length should equal(1)
  }

  it should "be able to return the list of tasks specified (2 - to avoid faking)" in {
    val p2 = new ScriptedPlan {
      add task "My task 1"
      add task "My task 2"
    }
    (p2.tasks)(0).description should equal("My task 1")
    (p2.tasks)(1).description should equal("My task 2")
    p2.tasks.length should equal(2)
  }

}
