package org.pigsaw.ccpm

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class PlanTest extends FlatSpec with Matchers {
    
  "A Plan" should "be constructable with an empty block" in {
    new Plan {}
  }
  
  it should "be able to accept a Task" in {
    val p = new Plan {
      Task("My task 1")
    }
  }
  
  it should "be able to return the list of tasks specified (1)" in {
    val p1 = new Plan {
      add task "My task"
    }
    println("Starting assertions")
    (p1.tasks)(0) should equal (Task("My task"))
    p1.tasks.length should equal (1)
  }
  
  it should "be able to return the list of tasks specified (2 - to avoid faking)" in {
    val p2 = new Plan {
      add task "My task 1"
      add task "My task 2"
    }
    (p2.tasks)(0) should equal (Task("My task 1"))
    (p2.tasks)(1) should equal (Task("My task 2"))
    p2.tasks.length should equal (2)
  }
  
  it should "be able to accept a Task with an ID and description" in {
    val p = new Plan {
      add task 't100 as "My task"
    }
    p.tasks.length should equal (1)
    (p.tasks)(0) should equal (Task('t100, "My task"))
  }
  
  it should "give a task with just an if the default description" in {
    val p = new Plan {
      add task 't22
    }
    (p.tasks)(0) should equal (Task('t22, Task.DefaultDescription))
  }
}
