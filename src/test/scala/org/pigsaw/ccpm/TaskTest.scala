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
  
  it should "detect another task with same id is a variation of it" in {
    val t1 = Task('t100, "First task")
    val t2 = Task('t100, "Another task")
    (t1 isAVariationOf t2) should equal (true)
  }
  
  it should "not say another task is a variation of it if they've got different ids" in {
    val t1 = Task('t100, "First task")
    val t2 = Task('t101, "Another task")
    (t1 isAVariationOf t2) should equal (false)
  }
  
  it should "have a default duration of zero" in {
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
  
  it should "allow the extraction of half duration" in {
    val t = Task('t100, 12)
    t.halfDuration should equal (6)
  }
  
  it should "be constructable with an optional resource" in {
    new Task('t100, "My task", 4, Some("Bob"))
  }
  
  it should "have a default resource of None" in {
    val t = Task('t100)
    t.resource should equal (None)
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
  
  it should "generate the next auto-id (1)" in {
    val id = Task.nextId(List('t5, 't3, 't4))
    id should equal ('t6)
  }
  
  it should "generate the next auto-id (2 - to avoid faking)" in {
    val id = Task.nextId(List('a, 't7, 'c))
    id should equal ('t8)
  }
  
  it should "generate the default id if no others are of the auto format" in {
    val id = Task.nextId(List('a, 'b, 'c))
    id should equal (Task.DefaultId)
  }
  
  it should "identify an auto-id" in {
    Task.isAutoId('t0) should be (true)
    Task.isAutoId('t0t) should be (false)
    Task.isAutoId('t564) should be (true)
    Task.isAutoId('x0) should be (false)
    Task.isAutoId('t) should be (false)
  }
}
