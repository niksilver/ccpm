package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class PlanTest extends FlatSpec with Matchers with ScheduleMatchers {
  
  "schedule" should "produce a sensible schedule" in {
    val p = new ScriptedPlan {
      declare resource "Alice"
      declare resource "Bob"
      add task 't1 as "Task one" duration 5 resource "Alice"
      add task 't2 as "Task two" duration 3 resource "Bob"
      't1 ~> 't2
    }
    val sch = p.schedule
    implicit val iSched = MatchingSchedule(sch)
    p.task('t1) should halfEndRightBefore (p.task('t2))
  }
  
  "chains" should "return paths if no resource conflicts" in {
    val p = new ScriptedPlan {
      add task 'a1 duration 1
      add task 'a2 duration 1
      add task 'b  duration 1
      add task 'c  duration 1
      add task 'd1 duration 1
      add task 'd2 duration 1
      'a1 ~> 'b ~> 'c ~> 'd1
      'a2 ~> 'b
      'c ~> 'd2
    }
    val a1 = p.task('a1)
    val a2 = p.task('a2)
    val b  = p.task('b)
    val c  = p.task('c)
    val d1 = p.task('d1)
    val d2 = p.task('d2)
    val chains = p.chains
    chains should contain (Seq(a1, b, c, d1))
    chains should contain (Seq(a1, b, c, d2))
    chains should contain (Seq(a2, b, c, d1))
    chains should contain (Seq(a2, b, c, d2))
    chains.length should equal (4)
  }

}