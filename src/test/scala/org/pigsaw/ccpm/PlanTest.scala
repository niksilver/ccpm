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
    t1 should endRightBefore (t2)
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

  it should "include adjacent resources in chains if there are any" in {
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
      val tasks = Seq(t1, t2, t3)
      val dependencies = Seq((t1 -> t3), (t2 -> t3))
    }
    val chains = p.chains
    chains should (
      contain (Seq(t2, t1, t3)) or
      contain (Seq(t1, t2, t3)))
    chains should (
      contain (Seq(t2, t3)) or
      contain (Seq(t1, t3)))
    chains.length should equal (2)
  }

  it should "include zero-length tasks in chains" in {
    val t1 = Task('t1, "Task one", 5, Some("Alice"))
    val t2 = Task('t2, "Task two", 4, Some("Alice"))
    val tEnd = Task('tEnd, "End", 0, None)

    // The schedule will be
    //             [t1 Alice]\                 [t1 Alice]----------\
    //   [t2 Alice]----------+[tEnd]     or              [t2 Alice]+[tEnd]
    //
    // The chains will be
    //             /[t1 Alice]\                [t1 Alice]+----------\
    //   [t2 Alice]+----------+[tEnd]    or              \[t2 Alice]+[tEnd]

    val p = new Plan {
      val tasks = Seq(t1, t2, tEnd)
      val dependencies = Seq((t1 -> tEnd), (t2 -> tEnd))
    }
    val chains = p.chains
    chains should (
      contain (Seq(t2, t1, tEnd)) or
      contain (Seq(t1, t2, tEnd)))
    chains should (
      contain (Seq(t2, tEnd)) or
      contain (Seq(t1, tEnd)))
    chains.length should equal (2)
  }

  it should "handle a mix of conflicting and non-conflicting tasks" in {
    val t1 = Task('t1, "Task one", 5, Some("Alice"))
    val t2 = Task('t2, "Task two", 5, Some("Alice"))
    val t3 = Task('t3, "Task three", 3, Some("Bob"))
    val t4 = Task('t4, "Task four", 5, Some("Alice"))

    // The schedule will be
    //             [t1  Alice]\
    //   [t2 Alice]---[t3 Bob]+[t4 Alice]
    //
    // The chains will be
    //             /[t1  Alice]\
    //   [t2 Alice]+---[t3 Bob]+[t4 Alice]

    val p = new Plan {
      val tasks = Seq(t1, t2, t3, t4)
      val dependencies = Seq((t1 -> t4), (t2 -> t3), (t3 -> t4))
    }
    val chains = p.chains
    
    chains should contain theSameElementsAs (Seq(
      Seq(t2, t1, t4),
      Seq(t2, t3, t4)))
  }

  it should "not include non-dependent, non-resource-adjacent tasks" in {
    val t1 = Task('t1, "Task one", 5, Some("Alice"))
    val t2 = Task('t2, "Task two", 5, Some("Alice"))
    val t3 = Task('t3, "Task three", 8, Some("Bob"))
    val t4 = Task('t4, "Task four", 5, Some("Alice"))

    // The schedule will be
    //                [t1 Alice]\
    //   [t2 Alice]-[t3     Bob]+[t4 Alice]
    //
    // The chains will be
    //                [t1 Alice]\
    //   [t2 Alice]-[t3     Bob]+[t4 Alice]

    val p = new Plan {
      val tasks = Seq(t1, t2, t3, t4)
      val dependencies = Seq((t1 -> t4), (t2 -> t3), (t3 -> t4))
    }
    val chains = p.chains
    
    chains should contain theSameElementsAs (Seq(
      Seq(t1, t4),
      Seq(t2, t3, t4)))
  }

  "criticalChain" should "extract the longest chain" in {
    val t1 = Task('t1, "Task one", 3, Some("Bob"))
    val t2 = Task('t2, "Task two", 5, Some("Alice"))
    val t3 = Task('t3, "Task three", 3, Some("Bob"))
    val t4 = Task('t4, "Task four", 5, Some("Alice"))
    val t5 = Task('t5, "Task five", 5, Some("Alice"))

    // The schedule will be
    //         [t1 Bob]\
    //   [t2  Alice]---+[t3 Bob]+[t4  Alice]
    //               [t5  Alice]/

    val p = new Plan {
      val tasks = Seq(t1, t2, t3, t4, t5)
      val dependencies = Seq((t1 -> t3), (t2 -> t3), (t3 -> t4), (t5 -> t4))
    }
    
    val chain = p.criticalChain
    chain should equal (Seq(t2, t5, t4))
    Chain(chain).length should equal (5+5+5)
  }
  
  it should "return the empty sequence if there are no chains at all (no tasks)" in {
      EmptyPlan.criticalChain should equal (Nil)
  }
  
  it should "work if there's only one task" in {
    val t0 = Task('t0, "The only one", 3, None)
    val p = new Plan {
      val tasks = Seq(t0)
      val dependencies = Nil
    }
    
    val chain = p.criticalChain
    chain should equal (Seq(t0))
    Chain(chain).length should equal (3)
  }
  
  "nonCriticalPaths" should "be empty for an empty plan" in {
      EmptyPlan.nonCriticalPaths should equal (Nil)
  }
}