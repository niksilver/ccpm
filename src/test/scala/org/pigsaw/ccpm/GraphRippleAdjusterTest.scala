package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class GraphRippleAdjusterTest extends FlatSpec with Matchers {
  // The network-raising problem is as follows.
  // We have a directed acyclic graph, where the direction
  // flows left to right. Each node has an ID and an integer score.
  // There is a constraint:
  // The score must be less than the score in any successor node.
  // We wish to increase (raise) the score of one of the nodes. But to
  // do this without violating the constraint we may have to first
  // raise the score in one or more successor nodes.
  // Also, it is forbidden to increase the score of a node which has
  // no successors.

  class Network(val edges: Set[(Symbol, Symbol)], val scores: Map[Symbol, Int])
    extends Graph[Symbol](edges) {
      
    def withScore(id: Symbol, score: Int) = {
      new Network(edges, scores + (id -> score))
    }
  }
  
  case class Move(id: Symbol, newScore: Int)
  
  class NetworkAdjuster extends RippleAdjuster[Network, Move] {
    def attempt(net: Network, move: Move): Seq[Attempt[Move]] = {
      val succs = net.successors(move.id)
      if (succs.isEmpty) {
        Seq()
      } else if (net.scores(succs.head) > move.newScore) {
    	Seq(Actual(move))
      } else {
        succs.toSeq filter { id => net.scores(id) <= move.newScore } map { id => Prerequisite(Move(id, move.newScore+1)) }
      }
    }
    def make(net: Network, move: Move): Network = {
      val succs = net.successors(move.id)
      if (succs.isEmpty) {
        net
      } else {
        val nextLimit = (succs map { net.scores(_) }).min
        if (move.newScore < nextLimit) {
          net.withScore(move.id, move.newScore)
        } else {
          net.withScore(move.id, nextLimit - 1)
        }
      }
    }
  }

  "Network.withScore" should "set the value of a node" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 8)
    val n = new Network(graph, scores)
    
    val n2 = n.withScore('a, 4)
    n2.scores('a) should equal (4)
  }
  
  "NetworkAdjuster.attempt" should "return no moves if node has no successors (1)" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 3)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    adjuster.attempt(n, Move('b, 4)) should equal (Seq())
  }
  
  it should "return a prerequisite if its single successor has to increment" in {
    val graph = Set('x -> 'y)
    val scores = Map('x -> 1, 'y -> 3)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    // If we want to increment x to 6, then y has to go to 7
    adjuster.attempt(n, Move('x, 6)) should equal (Seq(Prerequisite(Move('y, 7))))
  }
  
  it should "return prerequisites for all successors if they are all too low" in {
    val graph = Set('a -> 'b1, 'a -> 'b2, 'a -> 'b3)
    val scores = Map('a -> 1, 'b1 -> 3, 'b2 -> 4, 'b3 -> 5)
    val n = new Network(graph, scores)

    val adjuster = new NetworkAdjuster
    
    val att = adjuster.attempt(n, Move('a, 5))
    att should contain theSameElementsAs (Seq(
        Prerequisite(Move('b1, 6)),
        Prerequisite(Move('b2, 6)),
        Prerequisite(Move('b3, 6))))
  }
  
  it should "return prerequisites for some successors only some are too low" in {
    val graph = Set('a -> 'b1, 'a -> 'b2, 'a -> 'b3)
    val scores = Map('a -> 1, 'b1 -> 3, 'b2 -> 4, 'b3 -> 5)
    val n = new Network(graph, scores)

    val adjuster = new NetworkAdjuster
    
    val att = adjuster.attempt(n, Move('a, 4))
    att should contain theSameElementsAs (Seq(
        Prerequisite(Move('b1, 5)),
        Prerequisite(Move('b2, 5))))
  }
  
  it should "return an actual move if its single successor is sufficiently high" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 6)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    // If we want to increment a to 4, then that's fine
    adjuster.attempt(n, Move('a, 4)) should equal (Seq(Actual(Move('a, 4))))
  }
  
  "NetworkAdjuster.make" should "keep the score the same if it has no predecessors" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 6)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    val n2 = adjuster.make(n, Move('b, 7))
    n2.scores('a) should equal (1)
    n2.scores('b) should equal (6)
  }
  
  it should "increase to the max if the next (and only) node is high enough" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 4)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    val n2 = adjuster.make(n, Move('a, 3))
    n2.scores('a) should equal (3)
    n2.scores('b) should equal (4)
  }
  
  it should "increase partially if the next (and only) node is too low" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 4)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    val n2 = adjuster.make(n, Move('a, 6))
    n2.scores('a) should equal (3)
    n2.scores('b) should equal (4)
  }
  
  it should "increase partially if one of several next nodes is too low" in {
    val graph = Set('a -> 'b1, 'a -> 'b2, 'a -> 'b3)
    val scores = Map('a -> 1, 'b1 -> 8, 'b2 -> 6, 'b3 -> 4)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    val n2 = adjuster.make(n, Move('a, 6))
    n2.scores('a) should equal (3)
    n2.scores('b1) should equal (8)
    n2.scores('b2) should equal (6)
    n2.scores('b3) should equal (4)
  }
  
  "NetworkAdjuster.solve" should "work for a simple linear graph" in {
    val graph = Set('a -> 'b, 'b -> 'c, 'c -> 'd, 'd -> 'e)
    val scores = Map ('a -> 1, 'b -> 4, 'c -> 6, 'd -> 7, 'e -> 9)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    val n2 = adjuster.solve(n, Move('a, 6))
    n2.scores('a) should equal (5)
    n2.scores('b) should equal (6)
    n2.scores('c) should equal (7)
    n2.scores('d) should equal (8)
    n2.scores('e) should equal (9)
  }
}