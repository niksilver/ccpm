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
    (p1.tasks)(0) should equal(Task("My task"))
    p1.tasks.length should equal(1)
  }

  it should "be able to return the list of tasks specified (2 - to avoid faking)" in {
    val p2 = new Plan {
      add task "My task 1"
      add task "My task 2"
    }
    (p2.tasks)(0).description should equal("My task 1")
    (p2.tasks)(1).description should equal("My task 2")
    p2.tasks.length should equal(2)
  }

  it should "be able to accept a Task with an ID and description" in {
    val p = new Plan {
      add task 't100 as "My task"
    }
    p.tasks.length should equal(1)
    (p.tasks)(0) should equal(Task('t100, "My task"))
  }

  it should "give a task with just an id the default description" in {
    val p = new Plan {
      add task 't22
    }
    (p.tasks)(0) should equal(Task('t22, Task.DefaultDescription))
  }

  it should "reject a second task with the same id for tasks without descriptions" in {
    a[DuplicateTaskException] should be thrownBy {
      new Plan {
        add task 't33
        add task 't33
      }
    }
  }

  it should "reject a second task with the same id for tasks with descriptions" in {
    a[DuplicateTaskException] should be thrownBy {
      new Plan {
        add task 't33 as "Task one"
        add task 't33 as "Task two"
      }
    }
  }

  it should "generate unique task ids for tasks that need them" in {
    val p = new Plan {
      add task "Task one"
      add task 't13 as "Task two"
      add task "Task three"
      add task 't3 as "Task four"
      add task "Task five"
    }
    val tasks = p.tasks
    tasks.length should equal(5)
    tasks(0).id should equal('t0)
    tasks(1).id should equal('t13)
    tasks(2).id should equal('t14)
    tasks(3).id should equal('t3)
    tasks(4).id should equal('t15)
  }

  it should "allow task dependencies to be expressed by ids" in {
    new Plan {
      add task 't0
      add task 't1
      't0 ~> 't1
    }
  }

  it should "allow task dependencies to be expressed by chaining ids" in {
    new Plan {
      add task 't0
      add task 't1
      add task 't2
      't0 ~> 't1 ~> 't2
    }
  }

  it should "throw an exception if earlier task id does not exist" in {
    an[UnknownTaskException] should be thrownBy {
      new Plan {
        add task 't0
        add task 't1
        'xxx ~> 't1
      }
    }
  }

  it should "throw an exception if later task id does not exist" in {
    an[UnknownTaskException] should be thrownBy {
      new Plan {
        add task 't0
        add task 't1
        't0 ~> 'xxx
      }
    }
  }

  it should "allow extraction of dependencies" in {
    val p = new Plan {
      add task 't0
      add task 't1
      't0 ~> 't1
    }
    p.dependencies should contain(Task('t0) -> Task('t1))
  }

  it should "allow extraction of chained dependencies" in {
    val p = new Plan {
      add task 't0
      add task 't1
      add task 't2
      't0 ~> 't1 ~> 't2
    }
    p.dependencies should contain(Task('t0) -> Task('t1))
    p.dependencies should contain(Task('t1) -> Task('t2))
  }

  it should "reject cyclic dependencies" in {
    a[CyclicDependencyException] should be thrownBy {
      new Plan {
        add task 't0
        add task 't1
        add task 't2
        't0 ~> 't1
        't1 ~> 't2
        't2 ~> 't0
      }
    }
  }
  
  it should "allow specification of duration via the DSL" in {
    val p = new Plan {
      add task 't0 duration 5
    }
    p.task('t0).duration should equal (5)
  }
  
  it should "allow DSL syntax add task ... duration ... as ..." in {
    val p = new Plan {
      add task 't0 duration 5 as "First task"
    }
    p.task('t0).description should equal ("First task")
    p.task('t0).duration should equal (5)
  }
  
  it should "allow specification of a resource via the DSL" in {
    val p = new Plan {
      add task 't0 resource "Kevin"
    }
    p.task('t0).resource should equal (Some("Kevin"))
  }

}
