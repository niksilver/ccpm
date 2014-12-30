package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class GraphTest extends FlatSpec with Matchers {
  
  "Graph.remainsAcyclic" should "be true for an empty graph and ordinary edge" in {
    val g = List[(Symbol,Symbol)]()
    val edge = ('a -> 'b)
    Graph.remainsAcyclic(g, edge) should be (true)
  }
  
  it should "be false for an empty graph and a reflexive edge" in {
    val g = List[(Symbol,Symbol)]()
    val edge = ('a -> 'a)
    Graph.remainsAcyclic(g, edge) should be (false)
  }
  
  it should "be false for a non-empty graph and a reflexive edge" in {
    val g = List(('a -> 'b), ('b -> 'c), ('b -> 'd))
    val edge = ('a -> 'a)
    Graph.remainsAcyclic(g, edge) should be (false)
  }
  
  it should "be false for a non-empty graph and cycle-making edge" in {
    val g = List(('a -> 'b), ('b -> 'c), ('b -> 'd))
    val edge = ('d -> 'a)
    Graph.remainsAcyclic(g, edge) should be (false)
  }
  
  it should "be true for a non-empty graph and non-cycle-making edge" in {
    val g = List(('a -> 'b), ('b -> 'c), ('b -> 'd))
    val edge = ('a -> 'd)
    Graph.remainsAcyclic(g, edge) should be (true)
  }
  
}