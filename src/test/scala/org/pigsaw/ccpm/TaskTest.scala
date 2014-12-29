package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TaskTest extends FlatSpec with Matchers {

  "A Task" should "be constructable with a description" in {
    val t = Task("Password protection")
  }
  
  it should "be able to output its description" in {
    val t = Task("Password protection")
    t.description should equal ("Password protection")
  }
}