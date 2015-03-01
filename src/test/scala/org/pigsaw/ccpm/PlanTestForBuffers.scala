package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class PlanTestForBuffers extends FlatSpec with Matchers {

  "completionBuffer" should "give a buffer of appropriate duration (1)" in {
    val t1 = Task('t1, "Task one", 5, Some("Alice"))
    val t2 = Task('t2, "Task two", 4, Some("Alice"))
    val t3 = Task('t3, "Task three", 3, Some("Alice"))

    // The schedule will be
    //             [t1 Alice]\                     [t1 Alice]----------\
    //   [t2 Alice]----------+[t3 Alice]     or              [t2 Alice]+[t3 Alice]
    //
    // The chains will be
    //             /[t1 Alice]\                    [t1 Alice]+----------\
    //   [t2 Alice]+----------+[t3 Alice]    or              \[t2 Alice]+[t3 Alice]

    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set((t1 -> t3), (t2 -> t3))
    }
    val cb = p.completionBuffer
    cb.duration should equal ((5+4+3)/2)
  }

  it should "give a buffer of appropriate duration (2 - to avoid faking)" in {
    val t1 = Task('t1, "Task one", 9, Some("Alice"))
    val t2 = Task('t2, "Task two", 8, Some("Alice"))
    val t3 = Task('t3, "Task three", 7, Some("Alice"))

    // The schedule will be
    //             [t1 Alice]\                     [t1 Alice]----------\
    //   [t2 Alice]----------+[t3 Alice]     or              [t2 Alice]+[t3 Alice]
    //
    // The chains will be
    //             /[t1 Alice]\                    [t1 Alice]+----------\
    //   [t2 Alice]+----------+[t3 Alice]    or              \[t2 Alice]+[t3 Alice]

    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set((t1 -> t3), (t2 -> t3))
    }
    val cb = p.completionBuffer
    cb.duration should equal ((9+8+7)/2)
  }

  it should "give a buffer with a unique id" in {
    val t1 = Task('t1)

    val plan1 = new Plan {
      val tasks = Set(t1)
      val dependencies = Set[(Task, Task)]()
    }
    val usualBufferId = plan1.completionBuffer.id
    
    val t2 = Task(usualBufferId)
    val plan2 = new Plan {
      val tasks = Set(t2)
      val dependencies = Set[(Task, Task)]()
    }
    
    plan2.completionBuffer.id should not equal (usualBufferId)
  }

  it should "give a buffer with the correct predecessor (1)" in {
    val t1 = Task('t1, 1)
    val t2 = Task('t2, 5)
    val t3 = Task('t3, 3)

    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t1 -> t3, t2 -> t3)
    }
    val cb = p.completionBuffer
    cb.predecessor should equal (t3)
  }

  it should "give a buffer with the correct predecessor (2 - to avoid faking)" in {
    val t11 = Task('t11, 3)
    val t12 = Task('t12, 7)
    val t13 = Task('t13, 9)

    val p = new Plan {
      val tasks = Set(t11, t12, t13)
      val dependencies = Set(t11 -> t13, t12 -> t13)
    }
    val cb = p.completionBuffer
    cb.predecessor should equal (t13)
  }
  
  "feederBuffersNeeded" should "return the empty set if there's just a critical chain" in {
    val t1 = Task('t1, 1)
    val t2 = Task('t2, 5)
    val t3 = Task('t3, 3)

    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t1 -> t2, t2 -> t3)
    }

    p.feederBuffersNeeded should equal (Set())
  }
  
  it should "return the only task not on the CC with half its duration, assuming there is only one (1)" in {
    val t1 = Task('t1, 1) // Not on critical chain
    val t2 = Task('t2, 5)
    val t3 = Task('t3, 3)

    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t1 -> t3, t2 -> t3)
    }

    p.feederBuffersNeeded should equal (Set((t1, 0.5)))
  }
  
  it should "return the only task not on the CC with half its duration, assuming there is only one (2 - to avoid faking)" in {
    val t1 = Task('t1, 4) // Not on critical chain
    val t2 = Task('t2, 5)
    val t3 = Task('t3, 3)

    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t1 -> t3, t2 -> t3)
    }

    p.feederBuffersNeeded should equal (Set((t1, 2.0)))
  }
  
  "bufferedSchedule" should "include buffer at the end of the last task" in {
    val t1 = Task('t1, 1)
    val t2 = Task('t2, 5)
    val t3 = Task('t3, 3)

    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t1 -> t3, t2 -> t3)
    }
    
    val cb = p.completionBuffer
    val bs = p.bufferedSchedule
    bs.start(cb) should equal (bs.end(t3))
  }
}