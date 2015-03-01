package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class GraphTest extends FlatSpec with Matchers {
  
  "predecessors" should "be singleton for end in a one-edge graph" in {
    val g = new Graph(Set('a -> 'b))
    g.predecessors('b) should equal (Set('a))
  }
  
  it should "be empty for the start in a one-edge graph" in {
    val g = new Graph(Set('a -> 'b))
    g.predecessors('a) should be (Set())
  }
  
  it should "be give multiple nodes if appropriate" in {
    val g = new Graph(Set('a -> 'z, 'b -> 'z))
    g.predecessors('z) should be (Set('a, 'b))
  }
  
  it should "be empty if the given node is not in the graph" in {
    val g = new Graph(Set('a -> 'b))
    g.predecessors('z) should be (Set())
  }
  
  "remainsAcyclic" should "be true for an empty graph and ordinary edge" in {
    val g: Graph[Symbol] = new Graph(Set())
    val edge = ('a -> 'b)
    g.remainsAcyclic(edge) should be (true)
  }
  
  it should "be false for an empty graph and a reflexive edge" in {
    val g: Graph[Symbol] = new Graph(Set())
    val edge = ('a -> 'a)
    g.remainsAcyclic(edge) should be (false)
  }
  
  it should "be false for a non-empty graph and a reflexive edge" in {
    val g = new Graph(Set(('a -> 'b), ('b -> 'c), ('b -> 'd)))
    val edge = ('a -> 'a)
    g.remainsAcyclic(edge) should be (false)
  }
  
  it should "be false for a non-empty graph and cycle-making edge" in {
    val g = new Graph(Set(('a -> 'b), ('b -> 'c), ('b -> 'd)))
    val edge = ('d -> 'a)
    g.remainsAcyclic(edge) should be (false)
  }
  
  it should "be true for a non-empty graph and non-cycle-making edge" in {
    val g = new Graph(Set(('a -> 'b), ('b -> 'c), ('b -> 'd)))
    val edge = ('a -> 'd)
    g.remainsAcyclic(edge) should be (true)
  }
  
  "ends" should "be empty for an empty graph" in {
    val g = new Graph[Symbol](Set())
    g.ends should equal (Set())
  }
  
  it should "give the single end node if there's a single simple path" in {
    val g = new Graph(Set(('a -> 'b), ('b -> 'c), ('c -> 'd)))
    g.ends should equal (Set('d))
  }
  
  it should "give the single end node if there are multiple in-between routes" in {
    val g = new Graph(Set(('a -> 'b), ('b -> 'c), ('a -> 'c), ('c -> 'd)))
    g.ends should equal (Set('d))
  }
  
  it should "give multiple end nodes if there are any" in {
    val g = new Graph(Set(('a -> 'b), ('b -> 'c), ('c -> 'd), ('a -> 'e)))
    g.ends should contain theSameElementsAs (Set('d, 'e))
  }
  
  "starts" should "be empty for an empty graph" in {
    val g = new Graph[Symbol](Set())
    g.starts should equal (Set())
  }
  
  it should "give the single start node if there's a single simple path" in {
    val g = new Graph(Set(('a -> 'b), ('b -> 'c), ('c -> 'd)))
    g.starts should equal (Set('a))
  }
  
  it should "give the single start node if there are multiple in-between routes" in {
    val g = new Graph(Set(('a -> 'b), ('b -> 'c), ('c -> 'd), ('b -> 'd)))
    g.starts should equal (Set('a))
  }
  
  it should "give multiple start nodes if there are any" in {
    val g = new Graph(Set(('a -> 'b), ('b -> 'c), ('c -> 'd), ('z -> 'c)))
    g.starts should contain theSameElementsAs (Set('a, 'z))
  }
  
  "path" should "extract a single path when there is just one" in {
    val g = new Graph(Set(('a -> 'b), ('b -> 'c), ('c -> 'd)))
    g.paths should equal (Set(Seq('a, 'b, 'c, 'd)))
  }
  
  it should "extract two paths when two start points converge" in {
    val g = new Graph(Set(('a1 -> 'b), ('a2 -> 'b), ('b -> 'c), ('c -> 'd)))
    g.paths should contain theSameElementsAs Set(
        Seq('a1, 'b, 'c, 'd),
        Seq('a2, 'b, 'c, 'd))
  }
  
  it should "extract four paths when two start points converge, then diverge" in {
    val g = new Graph(Set(
        ('a1 -> 'b), ('a2 -> 'b),
        ('b -> 'c),
        ('c -> 'd1), ('c -> 'd2)))
    g.paths should contain (Seq('a1, 'b, 'c, 'd1))
    g.paths should contain (Seq('a2, 'b, 'c, 'd1))
    g.paths should contain (Seq('a1, 'b, 'c, 'd2))
    g.paths should contain (Seq('a2, 'b, 'c, 'd2))
    g.paths.size should equal (4)
  }
  
  it should "behave correctly if graph is empty" in {
    val g: Graph[Symbol] = new Graph(Set())
    g.paths should equal (Set.empty)
  }
  
  "hasEdge" should "return false for empty graph" in {
    val g: Graph[Symbol] = new Graph(Set())
    g.hasEdge('a -> 'b) should be (false)
  }
  
  it should "return true if the edge is burried in the graph" in {
    val g = new Graph(Set(('a -> 'b), ('b -> 'c), ('c -> 'd)))
    g.hasEdge('b -> 'c) should be (true)
  }
  
  it should "return false if the edge is not burried in the graph" in {
    val g = new Graph(Set(('a -> 'b), ('b -> 'c), ('c -> 'd)))
    g.hasEdge('x -> 'y) should be (false)
  }
  
  "pathsTo" should "give all nodes if we give the end node of a line" in {
    val g = new Graph(Set('a -> 'b))
    g.pathsTo('b) should equal (Set(Seq('a, 'b)))
  }
  
  it should "give the one path back from here if we give a node in the middle of a line (1)" in {
    val g = new Graph(Set('a -> 'b, 'b -> 'c))
    g.pathsTo('b) should equal (Set(Seq('a, 'b)))
  }
  
  it should "give the one path back from here if we give a node in the middle of a line (2 - longer path to avoid faking)" in {
    val g = new Graph(Set('a -> 'b, 'b -> 'c, 'c -> 'd))
    g.pathsTo('c) should equal (Set(Seq('a, 'b, 'c)))
  }
  
  it should "give multiple paths if there are several back from here" in {
    // a--b-\
    // c--d--e--f

    val g = new Graph(Set('a -> 'b, 'b -> 'e, 'e -> 'f, 'c -> 'd, 'd -> 'e))
    g.pathsTo('e) should equal (Set(Seq('a, 'b, 'e), Seq('c, 'd, 'e)))
  }
  
  "pathsTo(..., ignore)" should "allow us to terminate paths early" in {
    // a--b-\
    // c--d--e--f

    val g = new Graph(Set('a -> 'b, 'b -> 'e, 'e -> 'f, 'c -> 'd, 'd -> 'e))
    val excluded = { (sym: Symbol) => sym == 'a }
    g.pathsTo(Set('e), excluded) should equal (Set(Seq('b, 'e), Seq('c, 'd, 'e)))
  }
  
}