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
import org.scalatest.Inspectors

class PlanTestForBufferedSchedule extends FlatSpec with Matchers with Inspectors {
 
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
  
  it should "not have a feeder buffer of length zero" in {
    //    [t0]--------------\
    //     | |              [t1   ]--\
    //     | \-[t4        ]-/        |
    //     +---[t2                ]  |
    //                           \--[t3]
    
    val t0 = Task('t0)
    val t1 = Task('t1, "Task one", 2.0, None)
    val t2 = Task('t2, "Task two", 5.0, None)
    val t3 = Task('t3)
    val t4 = Task('t4, "Task four", 3.0, None)
    
    val p = new Plan {
      val tasks = Seq(t0, t1, t2, t3, t4)
      val dependencies = Set(
          t0 -> t1, t1 -> t3,
          t0 -> t2, t2 -> t3,
          t0 -> t4, t4 -> t1, t4 -> t3)
    }
    
    forAll (p.bufferedSchedule.feederBuffers.toSeq) { fb => fb.duration shouldBe > (0) }
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
  
  it should "be empty if there are no tasks" in {
    EmptyPlan.bufferedSchedule.periods.size should equal (0)
  }

}
