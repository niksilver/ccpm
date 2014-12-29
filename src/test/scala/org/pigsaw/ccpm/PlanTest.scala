package org.pigsaw.ccpm

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class PlanTest extends FlatSpec with Matchers {
  
  "A Plan" should "be constructable with an empty block" in {
    Plan {}
  }
}