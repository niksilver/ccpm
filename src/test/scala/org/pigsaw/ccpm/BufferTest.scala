package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class BufferTest extends FlatSpec with Matchers {

  "make(id, path, max)" should "set the duration to half the path length if there's time (1)" in {
    val t0 = Task('t0, 5)
    val t1 = Task('t1, 6)
    val t2 = Task('t2, 7)
    val b = Buffer.make('b0, Seq(t0, t1, t2), 100)
    b.duration should equal ((5+6+7)/2)
  }

  it should "set the duration to half the path length if there's time (2 - to avoid faking)" in {
    val t0 = Task('t0, 3)
    val t1 = Task('t1, 4)
    val t2 = Task('t2, 5)
    val b = Buffer.make('b0, Seq(t0, t1, t2), 100)
    b.duration should equal ((3+4+5)/2)
  }

  it should "set the duration to the given maximum if the path is too long" in {
    val t0 = Task('t0, 30)
    val t1 = Task('t1, 40)
    val t2 = Task('t2, 50)
    val b = Buffer.make('b0, Seq(t0, t1, t2), 45)
    b.duration should equal (45)
  }
  
  it should "set the id to the one given (1)" in {
    val t0 = Task('t0, 30)
    val b = Buffer.make('b0, Seq(t0), 60)
    b.id should equal ('b0)
  }
  
  it should "set the id to the one given (2 - to avoid faking)" in {
    val t0 = Task('t0, 30)
    val b = Buffer.make('b99, Seq(t0), 60)
    b.id should equal ('b99)
  }
  
  it should "set the predecessor task correctly (1)" in {
    val t0 = Task('t0, 5)
    val t1 = Task('t1, 6)
    val t2 = Task('t2, 7)
    val b = Buffer.make('b0, Seq(t0, t1, t2), 100)
    b.predecessor should equal (t2)
  }
  
  it should "set the predecessor task correctly (2 - to avoid faking)" in {
    val t0 = Task('t0, 5)
    val t1 = Task('t1, 6)
    val t9 = Task('t9, "Extra task", 7, Some("Alice"))
    val b = Buffer.make('b0, Seq(t0, t1, t9), 100)
    b.predecessor should equal (t9)
  }
}