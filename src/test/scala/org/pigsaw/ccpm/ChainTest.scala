package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class ChainTest extends FlatSpec with Matchers {

  "toSeq" should "extract the sequence" in {
    val s = Seq(Task('t1), Task('t2), Task('t3))
    val chain = Chain(s)
    chain.toSeq should equal (s)
  }
}