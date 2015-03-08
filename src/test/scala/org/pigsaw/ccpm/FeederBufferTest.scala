package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class FeederBufferTest extends FlatSpec with Matchers {

  "FeederBuffer.duration" should "give 0 if no paths" in {
    FeederBuffer.duration(Set()) should equal (0)
  }
  
  it should "give half the length of a single path if there's just one path" in {
    val t1 = Task('t1, 4)
    val t2 = Task('t2, 5)
    val paths = Set(Seq(t1, t2))
    FeederBuffer.duration(paths) should equal ((4.0 + 5.0)/2)
  }
  
  it should "give half the length of the longest path if there are several" in {
    val t1a = Task('t1a, 4)
    val t1b = Task('t1b, 5)
    val t2a = Task('t2a, 3)
    val t2b = Task('t2b, 4)
    val t3a = Task('t3a, 2)
    val t3b = Task('t3b, 8)
    val paths = Set(Seq(t1a, t1b), Seq(t2a, t2b), Seq(t3a, t3b))
    FeederBuffer.duration(paths) should equal ((t3a.duration + t3b.duration)/2)
  }
}