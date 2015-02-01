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
  
  "moveBack" should "be able to move back a simple task in a simple schedule" in {
    val t1 = Task('t1, 5)
    val t2 = Task('t2, 10)
    val t3 = Task('t3, 0)
    
    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t1 -> t3, t2 -> t3)
    }
    
    val t1End = p.schedule.end(t1)
    t1End should equal (p.schedule.start(t3))
    
    val sch2 = p.moveBack(t1, 2.5)
    val t1EndRevised = sch2.end(t1)
    t1EndRevised should equal (p.schedule.start(t3) - 2.5)
  }
  
  it should "move the task back as far as its predecessor (where it has just one close by)" in {
    val t1 = Task('t1, 5)
    val t2 = Task('t2, 10)
    val t3 = Task('t3, 0)
    
    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t1 -> t2, t2 -> t3)
      override lazy val schedule = new Schedule(Map(t1 -> 0, t2 -> 6.5, t3 -> 16.5))
    }
    
    // Let's check we've set this up right, and
    // confirm there really is a gap of 1.5 between t1 and t2
    val t1End = p.schedule.end(t1)
    val t2Start = p.schedule.start(t2)
    (t2Start - t1End) should equal (1.5)
    
    val sch2 = p.moveBack(t2, 4)
    val t2StartRevised = sch2.start(t2)
    t2StartRevised should equal (t1End)
  }
  
  it should "move the task back to the max if there's just one predecessor far back" in {
    val t1 = Task('t1, 5)
    val t2 = Task('t2, 10)
    val t3 = Task('t3, 0)
    
    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set(t1 -> t2, t2 -> t3)
      override lazy val schedule = new Schedule(Map(t1 -> 0, t2 -> 20, t3 -> 30))
    }
    
    // Let's check we've set this up right, and
    // confirm there really is a gap of more than 4 between t1 and t2
    val desiredMove = 4.0
    val t1End = p.schedule.end(t1)
    val t2Start = p.schedule.start(t2)
    (t2Start - t1End) should be > (desiredMove)
    
    val sch2 = p.moveBack(t2, desiredMove)
    val t2StartRevised = sch2.start(t2)
    t2StartRevised should equal (t2Start - desiredMove)
  }
  
  it should "move the task back to the closest predecessor, if it has has several and all are closer than the max" in {
    val t1 = Task('t1, 10)
    val t2 = Task('t2, 10)
    val t3 = Task('t3, 10)
    val t4 = Task('t4, 10)
    val tEnd = Task('tEnd, 10)
    
    val p = new Plan {
      val tasks = Set(t1, t2, t3, t4, tEnd)
      val dependencies = Set(t1 -> tEnd, t2 -> tEnd, t3 -> tEnd, t4 -> tEnd)
      override lazy val schedule = new Schedule(Map(t1 -> 0, t2 -> 1, t3 -> 2, t4 -> 3, tEnd -> 15))
    }

    // We have ends: t1 -> 10, t2 -> 11, t3 -> 12, t4 -> 13
    // and tEnd starts at 15.
    // So the gaps range from 5 down to 2
    
    val desiredMove = 6.0
    val sch2 = p.moveBack(tEnd, desiredMove)
    val tEndStartRevised = sch2.start(tEnd)
    tEndStartRevised should equal (sch2.end(t4))
  }
  
  it should "move the task back to the closest predecessor, if it has has several and only some are closer than the max" in {
    val t1 = Task('t1, 10)
    val t2 = Task('t2, 10)
    val t3 = Task('t3, 10)
    val t4 = Task('t4, 10)
    val tEnd = Task('tEnd, 10)
    
    val p = new Plan {
      val tasks = Set(t1, t2, t3, t4, tEnd)
      val dependencies = Set(t1 -> tEnd, t2 -> tEnd, t3 -> tEnd, t4 -> tEnd)
      override lazy val schedule = new Schedule(Map(t1 -> 0, t2 -> 1, t3 -> 2, t4 -> 3, tEnd -> 15))
    }

    // We have ends: t1 -> 10, t2 -> 11, t3 -> 12, t4 -> 13
    // and tEnd starts at 15.
    // So the gaps range from 5 down to 2
    
    val desiredMove = 3.5
    val sch2 = p.moveBack(tEnd, desiredMove)
    val tEndStartRevised = sch2.start(tEnd)
    tEndStartRevised should equal (sch2.end(t4))
  }
  
  it should "move the task back by the max, if it has has several predecessors all far away" in {
    val t1 = Task('t1, 10)
    val t2 = Task('t2, 10)
    val t3 = Task('t3, 10)
    val t4 = Task('t4, 10)
    val tEnd = Task('tEnd, 10)
    
    val p = new Plan {
      val tasks = Set(t1, t2, t3, t4, tEnd)
      val dependencies = Set(t1 -> tEnd, t2 -> tEnd, t3 -> tEnd, t4 -> tEnd)
      override lazy val schedule = new Schedule(Map(t1 -> 0, t2 -> 1, t3 -> 2, t4 -> 3, tEnd -> 15))
    }

    // We have ends: t1 -> 10, t2 -> 11, t3 -> 12, t4 -> 13
    // and tEnd starts at 15.
    // So the gaps range from 5 down to 2
    
    val tEndStart = p.schedule.start(tEnd)
    val desiredMove = 1.5
    val sch2 = p.moveBack(tEnd, desiredMove)
    val tEndStartRevised = sch2.start(tEnd)
    tEndStartRevised should equal (tEndStart - desiredMove)
  }
  
  it should "not move a task back if the whole period behind it resource-conflicts" in {
    val t1 = Task('t1, "Task one", 10, Some("Alice"))
    val t2 = Task('t2, "Task two", 10, Some("Alice"))
    val t3 = Task('t3, "Task three", 10, Some("Bob"))
    val tEnd = Task('tEnd, 0)
    
    // Schedule is:
    //            [t1 Alice]\
    // [t2 Alice]-[t3 Bob  ]+[tEnd]

    val p = new Plan {
      val tasks = Set(t1, t2, t3, tEnd)
      val dependencies = Set(t1 -> tEnd, t2 -> t3, t3 -> tEnd)
      override lazy val schedule = new Schedule(Map(t1 -> 10, t2 -> 0, t3 -> 10, tEnd -> 20))
    }
    
    val sch2 = p.moveBack(t1, 3)
    sch2.start(t1) should be (10)
  }
}