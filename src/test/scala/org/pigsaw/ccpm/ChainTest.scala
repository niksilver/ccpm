package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class ChainTest extends FlatSpec with Matchers {

  "toSeq" should "extract the sequence" in {
    val s = Seq(Task('t1), Task('t2), Task('t3))
    val chain = Chain(s)
    chain.toSeq should equal (s)
  }

  "length" should "give the total length of all durations in the chain (1)" in {
    val t1 = new Task('t1, "My task", 2, Some("Alice"))
    val t2 = new Task('t2, "Task 2", 3, Some("Bob"))
    val t3 = new Task('t3, "Task 3", 4, Some("Carol"))

    val chain = Chain(Seq(t1, t2, t3))
    chain.length should equal (2+3+4)
  }

  it should "give the total length of all durations in the chain (2 - to avoid faking)" in {
    val t1 = new Task('t1, "My task", 5, Some("Alice"))
    val t2 = new Task('t2, "Task 2", 4, Some("Bob"))
    val t3 = new Task('t3, "Task 3", 3, Some("Carol"))

    val chain = Chain(Seq(t1, t2, t3))
    chain.length should equal (5+4+3)
  }
  
  it should "give zero if the chain is empty" in {
    Chain(Nil).length should equal (0)
  }
  
  it should "give the task duration if there's only one task" in {
    val t0 = new Task('t0, "Only task", 5, Some("Alice"))

    val chain = Chain(Seq(t0))
    chain.length should equal (5)
  }
}