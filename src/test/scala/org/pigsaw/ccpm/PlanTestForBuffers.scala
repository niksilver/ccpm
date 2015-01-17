package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.Ignore

class PlanTestForBuffers extends FlatSpec with Matchers with ScheduleMatchers {

  "bufferedSchedule" should "produce empty schedule if no tasks" in {
    val p = EmptyPlan
    val bs = p.bufferedSchedule
    bs.tasks.size should be (0)
    bs.buffers.size should be (0)
  }
  
  it should "keep the task and add a single buffer at the end if there's a single task" in {
    val t1 = Task('t1, "My task", 3, Some("Alice"))
    val p = new Plan {
      val tasks = Set(t1)
      val dependencies = Set[(Task, Task)]()
    }
    val bs = p.bufferedSchedule
    bs.tasks should equal (Set(t1))
    bs.buffers.size should be (1)
  }
  
  it should "add buffer with ids distinct from each other and the tasks" in {
    val t1 = Task('t1, "Task one", 3, Some("Alice"))
    val t2 = Task('t2, "Task two", 3, Some("Bob"))
    val p = new Plan {
      val tasks = Set(t1, t2)
      val dependencies = Set(t1 -> t2)
    }
    val bs = p.bufferedSchedule
    
    bs.tasks should equal (Set(t1, t2))
    bs.buffers.size should be (1)
    
    val ids = ((bs.tasks ++ bs.buffers) map { _.id }).toSet
    ids should contain allOf ('t1, 't2)
    ids.size should equal (3)
  }
  
  it should "add buffers with unique ids even if tasks have buffer-like ids" in {
    val b0 = Task('b0, "Task one", 3, Some("Alice"))
    val b1 = Task('b1, "Task two", 3, Some("Bob"))
    val p = new Plan {
      val tasks = Set(b0, b1)
      val dependencies = Set(b0 -> b1)
    }
    val bs = p.bufferedSchedule
    
    bs.tasks should equal (Set(b0, b1))
    bs.buffers.size should be (1)
    
    val ids = ((bs.tasks ++ bs.buffers) map { _.id }).toSet
    ids should contain allOf ('b0, 'b1)
    ids.size should equal (3)
  }
}