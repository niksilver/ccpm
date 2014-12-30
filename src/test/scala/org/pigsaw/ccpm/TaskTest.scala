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
  
  it should "be able to take a symbolic name as an id" in {
    val t = Task('t, "My task")
  }
  
  it should "be able to retrieve its id" in {
    val t = Task('t, "My task")
    t.id should equal ('t)
  }
  
  it should "have a default id of t0" in {
    val t = Task("My task")
    t.id should equal ('t0)
  }
  
  "The Task object" should "give the default id" in {
    val t = Task("My task")
    t.id should equal (Task.DefaultId)    
  }
}
