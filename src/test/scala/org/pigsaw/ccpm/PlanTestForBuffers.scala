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
  
  it should "include a feeder buffer of the appropriate length" in {
    
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
    
    val feederBuffers = p.bufferedSchedule.feederBuffers
    feederBuffers.size should equal (1)
    
    feederBuffers.head.duration should equal ((t1.duration + t2.duration)/2)
    feederBuffers.head.predecessor should equal (t2)
  }
  
  it should "start a feeder buffer at an appropriate point (1)" in {
    
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
    
    val t2OriginalEnd = p.schedule.end(t2)
    
    val feederBuffer = p.bufferedSchedule.feederBuffers.head
    
    p.bufferedSchedule.start(feederBuffer) should equal (t2OriginalEnd - (t1.duration + t2.duration)/2)
  }
  
  it should "start a feeder buffer at an appropriate point (2 - to avoid faking)" in {
    
    //       [t1]-[t2 ]\
    //  [t3           ]-[t4 ]
    
    val t1 = Task('t1, 1.5) // Not on critical chain
    val t2 = Task('t2, 2.5) // Not on critical chain
    val t3 = Task('t3, 5)
    val t4 = Task('t4, 3)

    val p = new Plan {
      val tasks = Set(t1, t2, t3, t4)
      val dependencies = Set(t1 -> t2, t2 -> t4, t3 -> t4)
    }
    
    val t2OriginalEnd = p.schedule.end(t2)
    
    val feederBuffer = p.bufferedSchedule.feederBuffers.head
    
    p.bufferedSchedule.start(feederBuffer) should equal (t2OriginalEnd - (t1.duration + t2.duration)/2)
  }
  
  it should "reschedule the path going into a feeder buffer" in {
    
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
    
    val t1OriginalStart = p.schedule.start(t1)
    val t2OriginalStart = p.schedule.start(t2)
    val expectedBufferDuration = (t1.duration + t2.duration)/2
    
    p.bufferedSchedule.start(t1) should equal (t1OriginalStart - expectedBufferDuration)
    p.bufferedSchedule.start(t2) should equal (t2OriginalStart - expectedBufferDuration)
  }
  
  it should "reschedule more than one path going into a feeder buffer" in {
    
    //       [t1]-[t2 ]\
    //  [t3           ]-[t4 ]
    //      [t5]-[t6  ]/
    
    val t1 = Task('t1, 1) // Not on critical chain
    val t2 = Task('t2, 2) // Not on critical chain
    val t3 = Task('t3, 5)
    val t4 = Task('t4, 3)
    val t5 = Task('t5, 1) // Not on critical chain
    val t6 = Task('t6, 3) // Not on critical chain

    val p = new Plan {
      val tasks = Set(t1, t2, t3, t4, t5, t6)
      val dependencies = Set(t1 -> t2, t2 -> t4,
          t3 -> t4,
          t5 -> t6, t6 -> t4)
    }
    
    val t1OriginalStart = p.schedule.start(t1)
    val t2OriginalStart = p.schedule.start(t2)
    val t2ExpectedBufferDuration = (t1.duration + t2.duration)/2

    val t5OriginalStart = p.schedule.start(t5)
    val t6OriginalStart = p.schedule.start(t6)
    val t6ExpectedBufferDuration = (t5.duration + t6.duration)/2
    
    p.bufferedSchedule.start(t1) should equal (t1OriginalStart - t2ExpectedBufferDuration)
    p.bufferedSchedule.start(t2) should equal (t2OriginalStart - t2ExpectedBufferDuration)
    
    p.bufferedSchedule.start(t5) should equal (t5OriginalStart - t6ExpectedBufferDuration)
    p.bufferedSchedule.start(t6) should equal (t6OriginalStart - t6ExpectedBufferDuration)
  }
  
  it should "return one buffer if multiple paths meet at the same point" in {
    
    //       [t1]\
	//      [t2 ]-[t3 ]\
    //  [t4           ]-[t5 ]
    
    val t1 = Task('t1, 1) // Not on critical chain
    val t2 = Task('t2, 2) // Not on critical chain
    val t3 = Task('t3, 2) // Not on critical chain
    val t4 = Task('t4, 6)
    val t5 = Task('t5, 1)

    val p = new Plan {
      val tasks = Set(t1, t2, t3, t4, t5)
      val dependencies = Set(t1 -> t3, t2 -> t3, t3 -> t5, t4 -> t5)
    }

    p.bufferedSchedule.feederBuffers.size should equal (1)
  }
  
  it should "return a buffer of duration based on longest path if multiple paths meet at the same point" in {
    
    //       [t1]\
	//      [t2 ]+
	//     [t3  ]+
	//      [t4 ]+[t5 ]\
    //  [t6           ]-[t7]
    
    val t1 = Task('t1, 1)
    val t2 = Task('t2, 2)
    val t3 = Task('t3, 3)
    val t4 = Task('t4, 2)
    val t5 = Task('t5, 2)
    val t6 = Task('t6, 8)
    val t7 = Task('t7, 1)

    val p = new Plan {
      val tasks = Set(t1, t2, t3, t4, t5, t6, t7)
      val dependencies = Set(t1 -> t5, t2 -> t5, t3 -> t5, t4 -> t5, t5 -> t7,
          t6 -> t7)
    }

    p.bufferedSchedule.feederBuffers.size should equal (1)
    
    val buffer = p.bufferedSchedule.feederBuffers.head
    buffer.duration should equal ((t3.duration + t5.duration)/2)
  }
  
  it should "account for a feeder path already having a bit of buffer" in {
    //  0  1    2  3.5  4
    //     [t1]-[t2 ]--\
    //  [t3           ]-[t4 ]
    
    val t1 = Task('t1, 1)
    val t2 = Task('t2, 1.5)
    val t3 = Task('t3, 4)
    val t4 = Task('t4, 1)

    val p = new Plan {
      val tasks = Set(t1, t2, t3, t4)
      val dependencies = Set(t1 -> t2, t2 -> t4, t3 -> t4)
      override lazy val schedule = new Schedule(Map(
          t1 -> 1, t2 -> 2, t3 -> 0, t4 -> 4
      ))
    }
    
    val t1OriginalStart = p.schedule.start(t1)
    val t2OriginalStart = p.schedule.start(t2)
    val expectedBufferDuration = (t1.duration + t2.duration)/2
    val currentGap = p.schedule.start(t4) - p.schedule.end(t2)
    
    p.bufferedSchedule.start(t1) should equal (t1OriginalStart - expectedBufferDuration + currentGap)
    p.bufferedSchedule.start(t2) should equal (t2OriginalStart - expectedBufferDuration + currentGap)
  }
  
  it should "not allow a path to move forward" in {
    //  0  1    2  3.5  4  5   6
    //     [t1]-[t2 ]---------\
    //  [t3                  ]-[t4 ]
    
    val t1 = Task('t1, 1)
    val t2 = Task('t2, 1.5)
    val t3 = Task('t3, 6)
    val t4 = Task('t4, 1)

    val p = new Plan {
      val tasks = Set(t1, t2, t3, t4)
      val dependencies = Set(t1 -> t2, t2 -> t4, t3 -> t4)
      override lazy val schedule = new Schedule(Map(
          t1 -> 1, t2 -> 2, t3 -> 0, t4 -> 6
      ))
    }
    
    val t1OriginalStart = p.schedule.start(t1)
    val t2OriginalStart = p.schedule.start(t2)
    
    p.bufferedSchedule.start(t1) should equal (t1OriginalStart)
    p.bufferedSchedule.start(t2) should equal (t2OriginalStart)
  }
  
  it should "give buffers unique names" in {
    //     [t1  ]+
    //   [t2    ]+
    //    [t3   ]+
    //      [t4 ]+
    //    [t5   ]+[t6]
    
    val t1 = Task('t1, 3)
    val t2 = Task('t2, 5)
    val t3 = Task('t3, 4)
    val t4 = Task('t4, 2)
    val t5 = Task('t5, 4)
    val t6 = Task('t6, 1)
    
    val p = new Plan {
      val tasks = Set(t1, t2, t3, t4, t5, t6)
      val dependencies = Set(t1 -> t6, t2 -> t6, t3 -> t6, t4 -> t6, t5 -> t6)
    }
    
    // There should be four feeder buffers and one completion buffer
    val buffers = p.bufferedSchedule.buffers
    
    buffers.size should equal (5)
    val bufferIds = (buffers map {_.id}).toSet
    bufferIds.size should equal (5)
  }

}
