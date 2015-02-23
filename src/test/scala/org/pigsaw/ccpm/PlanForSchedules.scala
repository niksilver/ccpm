package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class PlanForSchedules extends FlatSpec with Matchers {
  
  "withSchedule" should "give the same plan with a given schedule" in {
    val t1 = Task('t1, 1.0)
    val t2 = Task('t2, 1.0)
    val p = new Plan {
      val tasks = Set(t1, t2)
      val dependencies = Set(t1 -> t2)
      override lazy val schedule = new Schedule(Map((t1 -> 0), (t2 -> 10.0)))
    }
    
    val sch2 = new Schedule(Map((t1 -> 0.6), (t2 -> 8.0)))
    val p2 = p.withSchedule(sch2)
    p2.schedule.start(t1) should equal (0.6)
    p2.schedule.start(t2) should equal (8.0)
    p2.tasks should equal (Set(t1, t2))
    p2.dependencies should equal (Set(t1 -> t2))
  }
}