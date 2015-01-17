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
  
  it should "be definable with just an id" in {
    Task('t101)
  }
  
  it should "have the default description from the Task object" in {
    Task('t101).description should equal (Task.DefaultDescription)
  }
  
  "isAVariationof" should "detect another task with same id is a variation of it" in {
    val t1 = Task('t100, "First task")
    val t2 = Task('t100, "Another task")
    (t1 isAVariationOf t2) should equal (true)
  }
  
  it should "not say another task is a variation of it if they've got different ids" in {
    val t1 = Task('t100, "First task")
    val t2 = Task('t101, "Another task")
    (t1 isAVariationOf t2) should equal (false)
  }
  
  "duration" should "have a default duration of zero" in {
    val t = Task('t100)
    t.duration should equal (0)
  }
  
  it should "allow the setting of the duration to be non-zero" in {
    val t = Task('t100, 2)
    t.duration should equal (2)
  }
  
  it should "allow the setting of the duration to be a non-integer" in {
    val t = Task('t100, 2.5)
    t.duration should equal (2.5)
  }
  
  "Task(..., resource)" should "be constructable with an optional resource" in {
    new Task('t100, "My task", 4, Some("Bob"))
  }
  
  it should "have a default resource of None" in {
    val t = Task('t100)
    t.resource should equal (None)
  }
  
  "sameResource" should "be false if either resource requirement is None" in {
    val t1 = Task('t1, "My task one", 4, Some("Alice"))
    val t2 = Task('t2, "My task two", 4, None)
    val t3 = Task('t3, "My task three", 4, None)
    
    t1.sameResource(t2) should equal (false)
    t2.sameResource(t1) should equal (false)
    t2.sameResource(t3) should equal (false)
  }
  
  it should "be true if both resource requirements are the same" in {
    val t1 = Task('t1, "My task one", 4, Some("Alice"))
    val t2 = Task('t2, "My task two", 5, Some("Alice"))
    
    t1.sameResource(t2) should equal (true)
  }
  
  it should "be false if both tasks have some, but different, resource requirements" in {
    val t1 = Task('t1, "My task one", 4, Some("Alice"))
    val t2 = Task('t2, "My task two", 4, Some("Bob"))
    
    t1.sameResource(t2) should equal (false)
  }
  
  "The Task object" should "give the default id" in {
    val t = Task("My task")
    t.id should equal (Task.DefaultId)
  }
  
  it should "have a sensible default id" in {
    Task.DefaultId should equal ('t0)
  }
  
  it should "give the sensible default description" in {
    Task.DefaultDescription should equal ("Anonymous task")
  }
  
  it should "give the sensible default duration" in {
    Task.DefaultDuration should equal (0)
  }
}
