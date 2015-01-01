package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class GraphTest extends FlatSpec with Matchers {
  
  "Graph.remainsAcyclic" should "be true for an empty graph and ordinary edge" in {
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
  
}