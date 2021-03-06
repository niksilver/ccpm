package org.pigsaw.ccpm

/* Copyright Nik Silver 2015.
 * 
 * This file is part of CCPM.
 *
 * CCPM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *  
 * CCPM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with CCPM.  If not, see <http://www.gnu.org/licenses/>.
 */

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
  
  "completionBufferOption" should "indicate there's no completion buffer if there are no tasks" in {
    val p = new Plan {
      val tasks = Set[Task]()
      val dependencies = Set[(Task, Task)]()
    }
    p.completionBufferOption should equal (None)
  }
  
  it should "give a completion buffer if there are some tasks" in {
    val t1 = Task('t1, 3)
    val t2 = Task('t2, 7)
    val t3 = Task('t3, 9)

    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t1 -> t3, t2 -> t3)
    }
    val cbo = p.completionBufferOption
    cbo shouldBe a [Some[_]]
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
  
  it should "return the only task not on the CC with the join and half its duration, assuming there is only one (1)" in {
    val t1 = Task('t1, 1) // Not on critical chain
    val t2 = Task('t2, 5)
    val t3 = Task('t3, 3)

    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t1 -> t3, t2 -> t3)
    }

    p.feederBuffersNeeded should equal (Set((t1, t3, 0.5)))
  }
  
  it should "return the only task not on the CC with the join and half its duration, assuming there is only one (2 - to avoid faking)" in {
    val t1 = Task('t1, 4) // Not on critical chain
    val t2 = Task('t2, 5)
    val t3 = Task('t3, 3)

    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t1 -> t3, t2 -> t3)
    }

    p.feederBuffersNeeded should equal (Set((t1, t3, 2.0)))
  }
  
  it should "return the last task of the only path not on the CC, with the join and half its duration" in {
    
    //       [t1]-[t2 ]\
    //  [t3           ]-[t4 ]
    
    val t1 = Task('t1, 1) // Not on critical chain
    val t2 = Task('t2, 2) // Not on critical chain
    val t3 = Task('t3, 5)
    val t4 = Task('t4, 3)

    val p = new Plan {
      val tasks = Set(t1, t2, t3, t4)
      val dependencies = Set(t1 -> t2, t2 -> t4, t3 -> t4)
    }

    p.feederBuffersNeeded should equal (Set((t2, t4, 1.5)))
  }
  
  it should "return just one task and the join, even if several paths lead into it" in {
    
    //       [t1a]+
    //      [t1b ]+
    //     [t1c  ]+
    //      [t1d ]+[t2  ]\
    //  [t3             ]-[t4 ]
    
    val t1a = Task('t1a, 1.0)
    val t1b = Task('t1b, 1.1)
    val t1c = Task('t1c, 1.2)
    val t1d = Task('t1d, 1.1)
    val t2 = Task('t2, 2)
    val t3 = Task('t3, 5)
    val t4 = Task('t4, 3)

    val p = new Plan {
      val tasks = Set(t1a, t1b, t1c, t1d, t2, t3, t4)
      val dependencies = Set(t1a -> t2, t1b -> t2, t1c -> t2, t1d -> t2,
          t2 -> t4, t3 -> t4)
    }

    val (task, join, _) = p.feederBuffersNeeded.head
    task should equal (t2)
    join should equal (t4)
  }
  
  it should "return half the duration of the longest, even if several paths lead into the CC" in {
    
    //       [t1a]+
    //      [t1b ]+
    //     [t1c  ]+
    //      [t1d ]+[t2  ]\
    //  [t3             ]-[t4 ]
    
    val t1a = Task('t1a, 1.0)
    val t1b = Task('t1b, 1.1)
    val t1c = Task('t1c, 1.2)
    val t1d = Task('t1d, 1.1)
    val t2 = Task('t2, 2)
    val t3 = Task('t3, 5)
    val t4 = Task('t4, 3)

    val p = new Plan {
      val tasks = Set(t1a, t1b, t1c, t1d, t2, t3, t4)
      val dependencies = Set(t1a -> t2, t1b -> t2, t1c -> t2, t1d -> t2,
          t2 -> t4, t3 -> t4)
    }

    val (_, _, duration) = p.feederBuffersNeeded.head
    duration should equal ((t1c.duration + t2.duration)/2)
  }
  
  it should "give several paths, when there are some, all with maximal duration" in {
    
    //       [t1a]+
    //      [t1b ]+
    //     [t1c  ]+
    //      [t1d ]+[t2  ]\
    //  [t3             ]-[t4 ]
    //     [t5a  ]+[t6  ]/
    //    [t5b   ]+
    //      [t5c ]+
    
    val t1a = Task('t1a, 1.0)
    val t1b = Task('t1b, 1.1)
    val t1c = Task('t1c, 1.2)
    val t1d = Task('t1d, 1.1)
    val t2 = Task('t2, 2)
    val t3 = Task('t3, 5)
    val t4 = Task('t4, 3)
    val t5a = Task('t5a, 1.3)
    val t5b = Task('t5b, 1.4)
    val t5c = Task('t5c, 1.2)
    val t6 = Task('t6, 2)

    val p = new Plan {
      val tasks = Set(t1a, t1b, t1c, t1d,
          t2, t3, t4,
          t5a, t5b, t5c, t6)
      val dependencies = Set(t1a -> t2, t1b -> t2, t1c -> t2, t1d -> t2,
          t2 -> t4, t3 -> t4,
          t5a -> t6, t5b -> t6, t5c -> t6,
          t6 -> t4)
    }

    p.feederBuffersNeeded should contain theSameElementsAs (Set(
        (t2, t4, (t1c.duration + t2.duration)/2),
        (t6, t4, (t5b.duration + t6.duration)/2)
    ))
  }
  
  it should "give two outputs for a path if it hits the critical chain twice" in {
    
    //    [t1 ]+-----+
    //  [t2   ]+[t3 ]-[t4 ]
    
    val t1 = Task('t1, 2)
    val t2 = Task('t2, 3)
    val t3 = Task('t3, 2)
    val t4 = Task('t4, 2)
    
    val p = new Plan {
      val tasks = Set(t1, t2, t3, t4)
      val dependencies = Set(t1 -> t3, t1 -> t4, t2 -> t3, t3 -> t4)
    }
    
    p.feederBuffersNeeded should contain theSameElementsAs (Set(
        (t1, t3, t1.duration / 2),
        (t1, t4, t1.duration / 2)
    ))
  }
  
  it should "not duplicate an output if there are several paths of the same length" in {
    
    //       [t1a]+
    //       [t1b]+
    //       [t1c]+[t2  ]\
    //  [t3             ]-[t4 ]
    
    val t1a = Task('t1a, 1.0)
    val t1b = Task('t1b, 1.0)
    val t1c = Task('t1c, 1.0)
    val t2 = Task('t2, 2)
    val t3 = Task('t3, 5)
    val t4 = Task('t4, 3)

    val p = new Plan {
      val tasks = Set(t1a, t1b, t1c, t2, t3, t4)
      val dependencies = Set(t1a -> t2, t1b -> t2, t1c -> t2,
          t2 -> t4, t3 -> t4)
    }

    p.feederBuffersNeeded should equal (Set((t2, t4, (t1a.duration + t2.duration)/2)))
  }
  
  "pathsToCriticalChain" should "be empty if there is just the critical chain" in {
    val t1 = Task('t1, 1)
    val t2 = Task('t2, 5)
    val t3 = Task('t3, 3)

    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t1 -> t2, t2 -> t3)
    }

    p.pathsToCriticalChain should equal (Set())
  }
  
  it should "give one path into the critical chain if there is just one" in {
    
    //       [t1]-[t2 ]\
    //  [t3           ]-[t4 ]
    
    val t1 = Task('t1, 1) // Not on critical chain
    val t2 = Task('t2, 2) // Not on critical chain
    val t3 = Task('t3, 5)
    val t4 = Task('t4, 3)

    val p = new Plan {
      val tasks = Set(t1, t2, t3, t4)
      val dependencies = Set(t1 -> t2, t2 -> t4, t3 -> t4)
    }
    
    p.pathsToCriticalChain should equal (Set(Seq(t1, t2, t4)))
  }
  
  "dependenciesWithBuffers" should "include all task dependencies" in {
    val t1 = Task('t1, 1)
    val t2 = Task('t2, 5)
    val t3 = Task('t3, 3)

    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t1 -> t2, t2 -> t3)
    }
    
    p.dependenciesWithBuffers should contain allOf((t1, t2), (t2, t3))
  }
  
  it should "include the dependency from the last task to the completion buffer" in {
    val t1 = Task('t1, 1)
    val t2 = Task('t2, 5)
    val t3 = Task('t3, 3)

    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t1 -> t2, t2 -> t3)
    }
    
    val cp = p.completionBuffer
    
    p.dependenciesWithBuffers should contain (t3, cp)
  }
  
  it should "include dependencies into feeder buffers" in {
    //   [t1]---\
    //   [t2   ]+[t3 ]
    
    val t1 = Task('t1, 1)
    val t2 = Task('t2, 5)
    val t3 = Task('t3, 3)

    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t1 -> t3, t2 -> t3)
    }
    
    p.bufferedSchedule.feederBuffers.size should equal (1)
    
    val fb = p.bufferedSchedule.feederBuffers.head
    
    p.dependenciesWithBuffers should contain (t1, fb)
  }
  
  it should "give no dependencies if there are no tasks" in {
    val p = new Plan {
      val tasks = Set()
      val dependencies = Set[(Task, Task)]()
    }
    p.dependenciesWithBuffers shouldBe empty
  }
  
  "periodsWithBuffers" should "list all the tasks in their original order" in {
    //    [t1]\
    //        +[t2]\
    //             +[t3]\
    //                  +[t4]
    
    val t1 = Task('t1, "Task one", 1, None)
    val t2 = Task('t2, "Task two", 1, None)
    val t3 = Task('t3, "Task three", 1, None)
    val t4 = Task('t4, "Task four", 1, None)

    val p = new Plan {
      val tasks = Seq(t1, t2, t3, t4)
      val dependencies = Set(t1 -> t2, t2 -> t3, t3 -> t4)
    }
    
    p.periodsWithBuffers.toSeq should contain inOrder (t1, t2, t3, t4)
  }
  
  it should "put the completion buffer at the end" in {
    //    [t1]\
    //        +[t2]\
    //             +[t3]\
    //                  +[t4]
    
    val t1 = Task('t1, "Task one", 1, None)
    val t2 = Task('t2, "Task two", 1, None)
    val t3 = Task('t3, "Task three", 1, None)
    val t4 = Task('t4, "Task four", 1, None)

    val p = new Plan {
      val tasks = Seq(t1, t2, t3, t4)
      val dependencies = Set(t1 -> t2, t2 -> t3, t3 -> t4)
    }
    
    val cp = p.completionBuffer
    
    p.periodsWithBuffers.toSeq should equal (Seq(t1, t2, t3, t4, cp))
  }
  
  "periodsWithBuffers" should "put a feeder buffer directly after its predecessor if the tasks are ordered" in {
    //    [t1]\
    //        +[t2]---------------\
    //                            |
    //    [t3  ]\                 |
    //          +[t4  ]-----------+
    //                            |
    //    [t5                    ]+
    //                            +[t6]
    
    val t1 = Task('t1, "Task one", 1, None)
    val t2 = Task('t2, "Task two", 1, None)
    val t3 = Task('t3, "Task three", 2, None)
    val t4 = Task('t4, "Task four", 2, None)
    val t5 = Task('t5, "Task five", 10, None)
    val t6 = Task('t6)

    val p = new Plan {
      val tasks = Seq(t1, t2, t3, t4, t5, t6)
      val dependencies = Set(t1 -> t2, t2 -> t6, t3 -> t4, t4 -> t6, t5 -> t6)
    }
    
    val bufferT2 = (p.bufferedSchedule.feederBuffers find { _.predecessor == t2 }).get
    val bufferT4 = (p.bufferedSchedule.feederBuffers find { _.predecessor == t4 }).get
    
    p.periodsWithBuffers.sliding(3).toSeq should contain (Seq(t1, t2, bufferT2))
    p.periodsWithBuffers.sliding(3).toSeq should contain (Seq(t3, t4, bufferT4))
  }
 
}
