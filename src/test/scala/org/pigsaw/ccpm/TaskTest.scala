package org.pigsaw.ccpm

import org.scalatest.FlatSpec

class TaskTest extends FlatSpec {

  "A Task" should "be constructable with a description" in {
    val t = Task("Password protection")
  }
}