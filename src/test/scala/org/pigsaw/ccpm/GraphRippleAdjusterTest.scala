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
    def attempt(net: Network, move: Move) = {
      val succs = net.successors(move.id)
      if (succs.isEmpty) {
        val thisScore = net.scores(move.id)
        Actual(Move(move.id, thisScore))
      } else if (net.scores(succs.head) > move.newScore) {
    	Actual(move)
      } else {
        val nextId = succs.head
        Prerequisite(Move(nextId, move.newScore + 1))
      }
    }
    def make(net: Network, move: Move): Network = {
      val succs = net.successors(move.id)
      if (succs.isEmpty) {
        net
      } else {
        val nextId = succs.head
        val nextScore = net.scores(nextId)
        if (move.newScore < nextScore) {
          net.withScore(move.id, move.newScore)
        } else {
          net.withScore(move.id, nextScore - 1)
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
  
  "NetworkAdjuster.attempt" should "return an actual non-move if node has no successors (1)" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 3)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    adjuster.attempt(n, Move('b, 4)) should equal (Actual(Move('b, 3)))
  }
  
  it should "return an actual non-move if node has no successors (2 - to avoid faking)" in {
    val graph = Set('x -> 'y)
    val scores = Map('x -> 1, 'y -> 3)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    adjuster.attempt(n, Move('y, 4)) should equal (Actual(Move('y, 3)))
  }
  
  it should "return a prerequisite if its single successor has to increment" in {
    val graph = Set('x -> 'y)
    val scores = Map('x -> 1, 'y -> 3)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    // If we want to increment x to 6, then y has to go to 7
    adjuster.attempt(n, Move('x, 6)) should equal (Prerequisite(Move('y, 7)))
  }
  
  it should "return an actual move if its single successor is sufficiently high" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 6)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    // If we want to increment a to 4, then that's fine
    adjuster.attempt(n, Move('a, 4)) should equal (Actual(Move('a, 4)))
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