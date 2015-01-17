package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class AutoIdingTest extends FlatSpec with Matchers {
  
  "nextId" should "generate the next auto-id (1)" in {
    val ider = new AutoIding("t")
    val id = ider.nextId(List('t5, 't3, 't4))
    id should equal ('t6)
  }
  
  it should "generate the next auto-id (2 - to avoid faking)" in {
    val ider = new AutoIding("t")
    val id = ider.nextId(List('a, 't7, 'c))
    id should equal ('t8)
  }
  
  it should "generate the next auto-id for another prefix" in {
    val ider = new AutoIding("b")
    val id = ider.nextId(List('a, 'b7, 'c))
    id should equal ('b8)
  }
  
  it should "generate the default id if no others are of the auto format" in {
    val ider = new AutoIding("t")
    val id = ider.nextId(List('a, 'b, 'c))
    id should equal (Task.DefaultId)
  }
  
  "isAutoId" should "identify an auto-id" in {
    val ider = new AutoIding("t")
    ider.isAutoId('t0) should be (true)
    ider.isAutoId('t0t) should be (false)
    ider.isAutoId('t564) should be (true)
    ider.isAutoId('x0) should be (false)
    ider.isAutoId('t) should be (false)
  }

}