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
    p1.tasks should equal (Set(Task("My task")))
  }

  it should "be able to return the list of tasks specified (2 - to avoid faking)" in {
    val p2 = new ScriptedPlan {
      add task "My task 1"
      add task "My task 2"
    }
    val taskSeq = p2.tasks.toSeq
    (taskSeq)(0).description should equal("My task 1")
    (taskSeq)(1).description should equal("My task 2")
    taskSeq.length should equal (2)
  }

  it should "return the tasks in order" in {
    val p2 = new ScriptedPlan {
      add task "My task 0"
      add task "My task 1"
      add task "My task 2"
      add task "My task 3"
      add task "My task 4"
      add task "My task 5"
    }
    val taskSeq = p2.tasks.toSeq
    (taskSeq)(0).description should equal("My task 0")
    (taskSeq)(1).description should equal("My task 1")
    (taskSeq)(2).description should equal("My task 2")
    (taskSeq)(3).description should equal("My task 3")
    (taskSeq)(4).description should equal("My task 4")
    (taskSeq)(5).description should equal("My task 5")
    taskSeq.length should equal (6)
  }

}
