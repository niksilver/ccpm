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
    
    def withInc(id: Symbol, inc: Int) = {
      val newScore = scores(id) + inc
      new Network(edges, scores + (id -> newScore))
    }
  }
  
  case class Move(id: Symbol, inc: Int)
  
  class NetworkAdjuster extends RippleAdjuster[Network, Move] {
    def attempt(net: Network, move: Move) = {
      val succs = net.successors(move.id)
      if (succs.isEmpty) {
        Actual(Move(move.id, 0))
      } else if (net.scores(succs.head) > net.scores(move.id) + move.inc) {
    	Actual(move)
      } else {
        val nextId = succs.head
        val nextScore = net.scores(nextId)
        val thisScore = net.scores(move.id)
        val nextInc = (thisScore + move.inc + 1) - nextScore
        Prerequisite(Move(nextId, nextInc))
      }
    }
    def make(net: Network, move: Move): Network = {
      val thisTargetScore = net.scores(move.id) + move.inc
      val succs = net.successors(move.id)
      if (succs.isEmpty) {
        net
      } else {
        val nextId = succs.head
        val nextScore = net.scores(nextId)
        if (thisTargetScore < nextScore) {
          new Network(net.edges, net.scores + (move.id -> thisTargetScore))
        } else {
          new Network(net.edges, net.scores + (move.id -> (nextScore - 1)))
        }
      }
    }
  }

  "Network.withInc" should "increment a given node" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 3)
    val n = new Network(graph, scores)
    
    val n2 = n.withInc('a, 7)
    n2.scores('a) should equal (1+7)
  }
  
  "NetworkAdjuster.attempt" should "return an actual non-move if node has no successors (1)" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 3)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    adjuster.attempt(n, Move('b, 1)) should equal (Actual(Move('b, 0)))
  }
  
  it should "return an actual non-move if node has no successors (2 - to avoid faking)" in {
    val graph = Set('x -> 'y)
    val scores = Map('x -> 1, 'y -> 3)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    adjuster.attempt(n, Move('y, 1)) should equal (Actual(Move('y, 0)))
  }
  
  it should "return a prerequisite if its single successor has to increment" in {
    val graph = Set('x -> 'y)
    val scores = Map('x -> 1, 'y -> 3)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    // If we want to increment x by 5 to 6, then y has to inc by 4
    adjuster.attempt(n, Move('x, 5)) should equal (Prerequisite(Move('y, 4)))
  }
  
  it should "return an actual move if its single successor is sufficiently high" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 6)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    // If we want to increment a by 3 to 4, then that's fine
    adjuster.attempt(n, Move('a, 3)) should equal (Actual(Move('a, 3)))
  }
  
  "NetworkAdjuster.make" should "keep the score the same if it has no predecessors" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 6)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    val n2 = adjuster.make(n, Move('b, 1))
    n2.scores('a) should equal (1)
    n2.scores('b) should equal (6)
  }
  
  it should "increase to the max if the next (and only) node is high enough" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 4)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    val n2 = adjuster.make(n, Move('a, 2))
    n2.scores('a) should equal (3)
    n2.scores('b) should equal (4)
  }
  
  it should "increase partially if the next (and only) node is too low" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 4)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    val n2 = adjuster.make(n, Move('a, 5))
    n2.scores('a) should equal (3)
    n2.scores('b) should equal (4)
  }
  
  "NetworkAdjuster.solve" should "work for a simple linear graph" in {
    val graph = Set('a -> 'b, 'b -> 'c, 'c -> 'd, 'd -> 'e)
    val scores = Map ('a -> 1, 'b -> 4, 'c -> 6, 'd -> 7, 'e -> 9)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    val n2 = adjuster.solve(n, Move('a, 5))
    n2.scores('a) should equal (5)
    n2.scores('b) should equal (6)
    n2.scores('c) should equal (7)
    n2.scores('d) should equal (8)
    n2.scores('e) should equal (9)
  }
}