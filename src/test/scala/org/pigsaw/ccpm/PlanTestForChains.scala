package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.Ignore

class PlanTestForChains extends FlatSpec with Matchers with ScheduleMatchers {

  "chains" should "return paths if no resource conflicts" in {
    val a1 = Task('a1, 1)
    val a2 = Task('a2, 1)
    val b = Task('b, 1)
    val c = Task('c, 1)
    val d1 = Task('d1, 1)
    val d2 = Task('d2, 1)
    val p = new Plan {
      val tasks = Set(a1, a2, b, c, d1, d2)
      val dependencies = Set(
        (a1 -> b), (a2 -> b),
        (b -> c),
        (c -> d1), (c -> d2))
    }
    val chains = p.chains
    chains should contain(Seq(a1, b, c, d1))
    chains should contain(Seq(a1, b, c, d2))
    chains should contain(Seq(a2, b, c, d1))
    chains should contain(Seq(a2, b, c, d2))
    chains.size should equal(4)
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
      val tasks = Set(t1, t2, t3)
      val dependencies = Set((t1 -> t3), (t2 -> t3))
    }
    val chains = p.chains
    chains should (
      contain(Seq(t2, t1, t3)) or
      contain(Seq(t1, t2, t3)))
    chains should (
      contain(Seq(t2, t3)) or
      contain(Seq(t1, t3)))
    chains.size should equal(2)
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
      val tasks = Set(t1, t2, tEnd)
      val dependencies = Set((t1 -> tEnd), (t2 -> tEnd))
    }
    val chains = p.chains
    chains should (
      contain(Seq(t2, t1, tEnd)) or
      contain(Seq(t1, t2, tEnd)))
    chains should (
      contain(Seq(t2, tEnd)) or
      contain(Seq(t1, tEnd)))
    chains.size should equal(2)
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
      val tasks = Set(t1, t2, t3, t4)
      val dependencies = Set((t1 -> t4), (t2 -> t3), (t3 -> t4))
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
      val tasks = Set(t1, t2, t3, t4)
      val dependencies = Set((t1 -> t4), (t2 -> t3), (t3 -> t4))
    }
    val chains = p.chains

    chains should contain theSameElementsAs (Seq(
      Seq(t1, t4),
      Seq(t2, t3, t4)))
  }

  it should "return the single-task chains if there are several tasks with no dependencies" in {
    val p = new ScriptedPlan {
      add task 't2
      add task 't4
      add task 't6
    }

    val t2 = p.task('t2)
    val t4 = p.task('t4)
    val t6 = p.task('t6)
    p.chains.size should equal(3)
    p.chains should contain theSameElementsAs (Set(Seq(t2), Seq(t4), Seq(t6)))
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
      val tasks = Set(t1, t2, t3, t4, t5)
      val dependencies = Set((t1 -> t3), (t2 -> t3), (t3 -> t4), (t5 -> t4))
    }

    val chain = p.criticalChain
    chain should equal(Seq(t2, t5, t4))
    Chain(chain).length should equal(5 + 5 + 5)
  }

  it should "return the empty sequence if there are no chains at all (no tasks)" in {
    EmptyPlan.criticalChain should equal(Nil)
  }

  it should "work if there's only one task" in {
    val t0 = Task('t0, "The only one", 3, None)
    val p = new Plan {
      val tasks = Set(t0)
      val dependencies = Set[(Task, Task)]()
    }

    val chain = p.criticalChain
    chain should equal(Seq(t0))
    Chain(chain).length should equal(3)
  }

  it should "return the longest task if there are several, with no dependencies" in {
    val t1 = Task('t1, "Task one", 17, None)
    val t2 = Task('t2, "Task two", 99, None)
    val t3 = Task('t3, "Task three", 34, None)

    val p = new Plan {
      val tasks = Set(t1, t2, t3)
      val dependencies = Set[(Task, Task)]()
    }

    val chain = p.criticalChain
    chain should equal (Seq(t2))
  }

  "feedOnCriticalChain" should "give task on critical chain, if path feeds into critical chain" in {

    //       /----[a1]+[a2]-[a3]-[a4]\
    //       |        \[i2]-[i3]/    |
    //       |                       |
    // [b1  ]+[b2       ]+[b3       ]+[b4   ]

    val (a1, a2, a3, a4) = (Task('a1, 2), Task('a2, 2), Task('a3, 2), Task('a4, 2))
    val (i2, i3) = (Task('i2, 2), Task('i3, 2))
    val (b1, b2, b3, b4) = (Task('b1, 5), Task('b2, 5), Task('b3, 5), Task('b4, 5))
    val p = new Plan {
      val tasks = Set(a1, a2, i2, i3, b1, b2, b3)
      val dependencies = Set(
        (b1 -> a1), (a1 -> a2), (a2 -> a3), (a3 -> a4), (a4 -> b4),
        (a1 -> i2), (i2 -> i3), (i3 -> a4),
        (b1 -> b2), (b2 -> b3), (b3 -> b4))
    }

    // The longer path feeds into the critical chain

    p.feedOnCriticalChain(Seq(a1, a2, a3, a4)) should equal(Some(b4))
    p.feedOnCriticalChain(Seq(a1, i2, i3, a4)) should equal(Some(b4))
  }

  it should "be empty for a path which does not feed into critical chain" in {

    //       /----[h1]+[h2]-[h3]-[h4]\
    //       |        \[i2]-[i3]/    |
    //       |                       |
    // [j1  ]+[j2       ]+[j3       ]+[j4   ]

    val (h1, h2, h3, h4) = (Task('h1, 2), Task('h2, 2), Task('h3, 2), Task('h4, 2))
    val (i2, i3) = (Task('i2, 2), Task('i3, 2))
    val (j1, j2, j3, j4) = (Task('j1, 5), Task('j2, 5), Task('j3, 5), Task('j4, 5))
    val p = new Plan {
      val tasks = Set(h1, h2, i2, i3, j1, j2, j3)
      val dependencies = Set(
        (j1 -> h1), (h1 -> h2), (h2 -> h3), (h3 -> h4), (h4 -> j4),
        (h1 -> i2), (i2 -> i3), (i3 -> h4),
        (j1 -> j2), (j2 -> j3), (j3 -> j4))
    }

    // The longer path feeds into the critical chain

    p.feedOnCriticalChain(Seq(h2, h3)) should equal(None)
    p.feedOnCriticalChain(Seq(i2, i3)) should equal(None)
  }

}
