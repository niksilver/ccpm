package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class DSLTest extends FlatSpec with Matchers {

  "The DSL for adding tasks" should "be able to accept a Task with an ID and description" in {
    val p = new ScriptedPlan {
      add task 't100 as "My task"
    }
    (p.tasks) should equal (Set(Task('t100, "My task")))
  }

  it should "give a task with just an id the default description" in {
    val p = new ScriptedPlan {
      add task 't22
    }
    (p.tasks) should equal (Set(Task('t22, Task.DefaultDescription)))
  }

  it should "reject a second task with the same id for tasks without descriptions" in {
    a[DuplicateTaskException] should be thrownBy {
      new ScriptedPlan {
        add task 't33
        add task 't33
      }
    }
  }

  it should "reject a second task with the same id for tasks with descriptions" in {
    a[DuplicateTaskException] should be thrownBy {
      new ScriptedPlan {
        add task 't33 as "Task one"
        add task 't33 as "Task two"
      }
    }
  }

  it should "generate unique task ids for tasks that need them" in {
    val p = new ScriptedPlan {
      add task "Task one"
      add task 't13 as "Task two"
      add task "Task three"
      add task 't3 as "Task four"
      add task "Task five"
    }
    val taskSeq = p.tasks.toSeq
    taskSeq.length should equal(5)
    taskSeq(0).id should equal('t0)
    taskSeq(1).id should equal('t13)
    taskSeq(2).id should equal('t14)
    taskSeq(3).id should equal('t3)
    taskSeq(4).id should equal('t15)
  }

  "The DSL for dependencies" should "allow task dependencies to be expressed by ids" in {
    new ScriptedPlan {
      add task 't0
      add task 't1
      't0 ~> 't1
    }
  }

  it should "allow task dependencies to be expressed by chaining ids" in {
    new ScriptedPlan {
      add task 't0
      add task 't1
      add task 't2
      't0 ~> 't1 ~> 't2
    }
  }

  it should "throw an exception if earlier task id does not exist" in {
    an[UnknownTaskException] should be thrownBy {
      new ScriptedPlan {
        add task 't0
        add task 't1
        'xxx ~> 't1
      }
    }
  }

  it should "throw an exception if later task id does not exist" in {
    an[UnknownTaskException] should be thrownBy {
      new ScriptedPlan {
        add task 't0
        add task 't1
        't0 ~> 'xxx
      }
    }
  }

  it should "allow extraction of dependencies" in {
    val p = new ScriptedPlan {
      add task 't0
      add task 't1
      't0 ~> 't1
    }
    p.dependencies should contain(Task('t0) -> Task('t1))
  }

  it should "allow extraction of chained dependencies" in {
    val p = new ScriptedPlan {
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
      new ScriptedPlan {
        add task 't0
        add task 't1
        add task 't2
        't0 ~> 't1
        't1 ~> 't2
        't2 ~> 't0
      }
    }
  }
  
  it should "reject a repeated dependency" in {
    a[DuplicateDependencyException] should be thrownBy {
      new ScriptedPlan {
        add task 't0
        add task 't1
        add task 't2
        add task 't3
        't0 ~> 't1
        't1 ~> 't2
        't2 ~> 't3
        't1 ~> 't2
      }
    }
  }

  "The DSL for task duration" should "allow specification of duration via the DSL" in {
    val p = new ScriptedPlan {
      add task 't0 duration 5
    }
    p.task('t0).duration should equal(5)
  }

  it should "allow syntax add task ... duration ... as ..." in {
    val p = new ScriptedPlan {
      add task 't0 duration 5 as "First task"
    }
    p.task('t0).description should equal("First task")
    p.task('t0).duration should equal(5)
  }

  it should "allow syntax add task ... as ... duration ..." in {
    val p = new ScriptedPlan {
      add task 't0 as "First task" duration 5
    }
    p.task('t0).description should equal("First task")
    p.task('t0).duration should equal(5)
  }

  "The DSL for resources" should "allow declaring of resources" in {
    new ScriptedPlan {
      declare resource "Alice"
    }
  }

  it should "ensure declared resources can be retrieved" in {
    val p = new ScriptedPlan {
      declare resource "Alice"
      declare resource "Bob"
    }
    p.resources.size should equal(2)
    p.resources should contain("Alice")
    p.resources should contain("Bob")
  }

  it should "reject any task's resource that's not been declared" in {
    a[UnknownResourceException] should be thrownBy {
      new ScriptedPlan {
        add task 't0 resource "Alice" as "First task"
      }
    }
  }

  it should "allow specification of a resource for a task via the DSL" in {
    val p = new ScriptedPlan {
      declare resource "Kevin"
      add task 't0 resource "Kevin"
    }
    p.task('t0).resource should equal(Some("Kevin"))
  }

  it should "specify the errant resource if a task uses an undeclared resource" in {
    val exc = the [UnknownResourceException] thrownBy {
    new ScriptedPlan {
      add task 't0 resource "Alice"
    }
    }
    exc.getMessage() should include ("Alice")
  }

  it should "allow DSL syntax add task ... resource ... as ..." in {
    val p = new ScriptedPlan {
      declare resource "Kevin"
      add task 't0 resource "Kevin" as "First task"
    }
    p.task('t0).description should equal("First task")
    p.task('t0).duration should equal(0)
    p.task('t0).resource should equal(Some("Kevin"))
  }

}
