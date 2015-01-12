package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class PlanTest extends FlatSpec with Matchers with ScheduleMatchers {

  "resources" should "return all resources" in {
    val t1 = Task('t1, "Task one", 1, Some("Alice"))
    val t2 = Task('t2, "Task two", 1, Some("Bob"))
    val t3 = Task('t3, "Task three", 1, Some("Carol"))
    val p = new Plan {
      val tasks = Seq(t1, t2, t3)
      val dependencies = Seq((t1 -> t2), (t2 -> t3))
    }
    p.resources should contain theSameElementsAs Seq("Alice", "Bob", "Carol")
  }

  it should "not repeat resources, even if they're in several tasks" in {
    val t1 = Task('t1, "Task one", 1, Some("Alice"))
    val t2 = Task('t2, "Task two", 1, Some("Alice"))
    val t3 = Task('t3, "Task three", 1, Some("Carol"))
    val p = new Plan {
      val tasks = Seq(t1, t2, t3)
      val dependencies = Seq((t1 -> t2), (t2 -> t3))
    }
    p.resources should contain theSameElementsAs Seq("Alice", "Carol")
  }

  "schedule" should "produce a sensible schedule" in {
    val t1 = Task('t1, "Task one", 5, Some("Alice"))
    val t2 = Task('t2, "Task two", 3, Some("Bob"))
    val p = new Plan {
      val tasks = Seq(t1, t2)
      val dependencies = Seq((t1 -> t2))
    }
    val sch = p.schedule
    implicit val iSched = MatchingSchedule(sch)
    t1 should halfEndRightBefore (t2)
  }

  "chains" should "return paths if no resource conflicts" in {
    val a1 = Task('a1, 1)
    val a2 = Task('a2, 1)
    val b = Task('b, 1)
    val c = Task('c, 1)
    val d1 = Task('d1, 1)
    val d2 = Task('d2, 1)
    val p = new Plan {
      val tasks = Seq(a1, a2, b, c, d1, d2)
      val dependencies = Seq(
        (a1 -> b), (a2 -> b),
        (b -> c),
        (c -> d1), (c -> d2))
    }
    val chains = p.chains
    chains should contain (Seq(a1, b, c, d1))
    chains should contain (Seq(a1, b, c, d2))
    chains should contain (Seq(a2, b, c, d1))
    chains should contain (Seq(a2, b, c, d2))
    chains.length should equal (4)
  }

}