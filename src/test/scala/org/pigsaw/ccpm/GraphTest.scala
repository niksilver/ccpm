package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class GraphTest extends FlatSpec with Matchers {
  
  "remainsAcyclic" should "be true for an empty graph and ordinary edge" in {
    val g: Graph[Symbol] = new Graph(Nil)
    val edge = ('a -> 'b)
    g.remainsAcyclic(edge) should be (true)
  }
  
  it should "be false for an empty graph and a reflexive edge" in {
    val g: Graph[Symbol] = new Graph(Nil)
    val edge = ('a -> 'a)
    g.remainsAcyclic(edge) should be (false)
  }
  
  it should "be false for a non-empty graph and a reflexive edge" in {
    val g = new Graph(List(('a -> 'b), ('b -> 'c), ('b -> 'd)))
    val edge = ('a -> 'a)
    g.remainsAcyclic(edge) should be (false)
  }
  
  it should "be false for a non-empty graph and cycle-making edge" in {
    val g = new Graph(List(('a -> 'b), ('b -> 'c), ('b -> 'd)))
    val edge = ('d -> 'a)
    g.remainsAcyclic(edge) should be (false)
  }
  
  it should "be true for a non-empty graph and non-cycle-making edge" in {
    val g = new Graph(List(('a -> 'b), ('b -> 'c), ('b -> 'd)))
    val edge = ('a -> 'd)
    g.remainsAcyclic(edge) should be (true)
  }
  
  "ends" should "be empty for an empty graph" in {
    val g = new Graph(Nil)
    g.ends should equal (Nil)
  }
  
  it should "give the single end node if there's a single simple path" in {
    val g = new Graph(List(('a -> 'b), ('b -> 'c), ('c -> 'd)))
    g.ends should equal (Seq('d))
  }
  
  it should "give the single end node if there are multiple in-between routes" in {
    val g = new Graph(List(('a -> 'b), ('b -> 'c), ('a -> 'c), ('c -> 'd)))
    g.ends should equal (Seq('d))
  }
  
  "starts" should "be empty for an empty graph" in {
    val g = new Graph(Nil)
    g.starts should equal (Nil)
  }
  
  it should "give the single start node if there's a single simple path" in {
    val g = new Graph(List(('a -> 'b), ('b -> 'c), ('c -> 'd)))
    g.starts should equal (Seq('a))
  }
  
  it should "give the single start node if there are multiple in-between routes" in {
    val g = new Graph(List(('a -> 'b), ('b -> 'c), ('c -> 'd), ('b -> 'd)))
    g.starts should equal (Seq('a))
  }
  
}