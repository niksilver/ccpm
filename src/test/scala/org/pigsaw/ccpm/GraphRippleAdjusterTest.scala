package org.pigsaw.ccpm

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class GraphRippleAdjusterTest extends FlatSpec with Matchers {
  // The network-raising problem is as follows.
  // We have a directed acyclic graph, where the direction
  // flows left to right. Each node has an ID and an integer score.
  // There is a constraint:
  // The score must be less than the score in any successor node
  // We wish to increase (raise) the score of one of the nodes. But to
  // do this without violating the constraint we may have to first
  // raise the score in one or more successor nodes.
  // Also, it is forbidden to increase the score of a node which has
  // no successors.
  // As a special scenario, we can insist the different between one
  // node's score and its successor is more than one. That's the
  // case when the earlier node's ID is of the form 'x_N where N is a digit
  // and the minimum difference.

  class Network(val edges: Set[(Symbol, Symbol)], val scores: Map[Symbol, Int])
    extends Graph[Symbol](edges) {

    def withScore(id: Symbol, score: Int) = {
      new Network(edges, scores + (id -> score))
    }
  }
  
  case class Move(id: Symbol, newScore: Int) extends RippleMove[Move] {
    def samePiece(m2: Move): Boolean = (this.id == m2.id)
    def max(m2: Move): Move = Seq(this, m2).maxBy(_.newScore)
    def size = {
      val idLen = id.name.length
      if (idLen >= 2 && id.name(idLen-2) == '_') {
        id.name(idLen-1).toString.toInt
      } else {
        1
      }
    }
  }

  class NetworkAdjuster extends RippleAdjuster[Network, Move] {
    def attempt(net: Network, move: Move): Seq[Attempt[Move]] = {
      val succs = net.successors(move.id)
      if (succs.isEmpty) {
        Seq()
      } else {
        val extent = move.newScore + move.size
        val nextLimit = (succs map { net.scores(_) }).min
        if (nextLimit >= extent) {
          Seq(Actual(move))
        } else {
          val prereqNodes = succs.toSeq filter { id => net.scores(id) < extent }
          prereqNodes map { id => Prerequisite(Move(id, extent)) }
        }
      }
    }

    def move(net: Network, move: Move): (Network, Move) = {
      val succs = net.successors(move.id)
      if (succs.isEmpty) {
        (net, move)
      } else {
        val nextLimit = (succs map { net.scores(_) }).min
        if (move.newScore + move.size <= nextLimit) {
          val actualMove = Move(move.id, move.newScore)
          val net2 = net.withScore(move.id, move.newScore)
          (net2, actualMove)
        } else {
          val actualMove = Move(move.id, nextLimit - move.size)
          val net2 = net.withScore(move.id, nextLimit - move.size)
          (net2, actualMove)
        }
      }
    }
  }
  
  "Move.size" should "have default value 1" in {
    Move('a, 3).size should equal (1)
  }
  
  it should "give another int when its name is _N" in {
    Move('a_4, 3).size should equal (4)
  }
  
  "Move.max" should "give max if greater piece is first" in {
    Move('a, 10).max(Move('a, 3)) should equal (Move('a, 10))
  }
  
  it should "give max if greater piece is second" in {
    Move('a, 3).max(Move('a, 10)) should equal (Move('a, 10))
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
  
  it should "return an actual move if a long node has a single successor which is sufficiently high" in {
    val graph = Set('a_4 -> 'b)
    val scores = Map('a_4 -> 1, 'b -> 6)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    adjuster.attempt(n, Move('a_4, 2)) should equal (Seq(Actual(Move('a_4, 2))))
  }
  
  it should "return a prerequisite if a long node has a single successor which is not high enough" in {
    val graph = Set('a_4 -> 'b)
    val scores = Map('a_4 -> 1, 'b -> 5)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    adjuster.attempt(n, Move('a_4, 2)) should equal (Seq(Prerequisite(Move('b, 6))))
  }
  
  "NetworkAdjuster.move" should "keep the score the same if it has no predecessors" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 6)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    val (n2, _) = adjuster.move(n, Move('b, 7))
    n2.scores('a) should equal (1)
    n2.scores('b) should equal (6)
  }
  
  it should "increase to the max if the next (and only) node is high enough" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 4)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    val (n2, _) = adjuster.move(n, Move('a, 3))
    n2.scores('a) should equal (3)
    n2.scores('b) should equal (4)
  }
  
  it should "increase partially if the next (and only) node is too low" in {
    val graph = Set('a -> 'b)
    val scores = Map('a -> 1, 'b -> 4)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    val (n2, _) = adjuster.move(n, Move('a, 6))
    n2.scores('a) should equal (3)
    n2.scores('b) should equal (4)
  }
  
  it should "increase partially if one of several next nodes is too low" in {
    val graph = Set('a -> 'b1, 'a -> 'b2, 'a -> 'b3)
    val scores = Map('a -> 1, 'b1 -> 8, 'b2 -> 6, 'b3 -> 4)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    val (n2, _) = adjuster.move(n, Move('a, 6))
    n2.scores('a) should equal (3)
    n2.scores('b1) should equal (8)
    n2.scores('b2) should equal (6)
    n2.scores('b3) should equal (4)
  }
  
  it should "make a full move for a long node if its successor is sufficiently high" in {
    val graph = Set('a_4 -> 'b)
    val scores = Map('a_4 -> 1, 'b -> 6)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    val (n2, m) = adjuster.move(n, Move('a_4, 2))
    m should equal (Move('a_4, 2))
    n2.scores('a_4) should equal (2)
    n2.scores('b) should equal (6)
  }
  
  it should "make a partial move for a long node if its successor is not sufficiently high" in {
    val graph = Set('a_4 -> 'b)
    val scores = Map('a_4 -> 1, 'b -> 6)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    
    val (n2, m) = adjuster.move(n, Move('a_4, 3))
    m should equal (Move('a_4, 2))
    n2.scores('a_4) should equal (2)
    n2.scores('b) should equal (6)
  }
  
  "RippleAdjuster.desiredMoves" should "return different moves for a simple linear graph with a high end node" in {
    val graph = Set('a -> 'b, 'b -> 'c)
    val scores = Map ('a -> 1, 'b -> 2, 'c -> 6)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    val moves = adjuster.desiredMoves(n, Move('a, 3))
    moves should contain theSameElementsAs (List(Move('b, 4), Move('a, 3)))
  }
  
  it should "return different moves for a simple linear graph with an insuffienctly high end node" in {
    val graph = Set('a -> 'b, 'b -> 'c)
    val scores = Map ('a -> 1, 'b -> 2, 'c -> 4)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    val moves = adjuster.desiredMoves(n, Move('a, 3))
    moves should contain theSameElementsAs (List(Move('c, 5), Move('b, 4), Move('a, 3)))
  }
  
  it should "allow for two branches, one with prerequisites and one allowing full moves" in {
    //      /-[b1 4]-[c1 5]-[d1 10]
    // [a 2]
    //      \-[b2 4]-[c2 10]

    val graph = Set('a -> 'b1, 'b1 -> 'c1, 'c1 -> 'd1,
      'a -> 'b2, 'b2 -> 'c2)
    val scores = Map ('a -> 2,
      'b1 -> 4, 'b2 -> 4,
      'c1 -> 5, 'c2 -> 10,
      'd1 -> 10)
    val n = new Network(graph, scores)

    val adjuster = new NetworkAdjuster
    val moves = adjuster.desiredMoves(n, Move('a, 4))
    
    moves should contain theSameElementsAs (List(
        Move('c1, 6),
        Move('b2, 5),
        Move('b1, 5),
        Move('a, 4)
    ))
  }
  
  it should "handle merging branches which different demands on the merged branch" in {
    //      /-[b11 4]-[b12 7]-\
    // [a 2]                   [c 7]-[d 10]
    //      \-[b2 6]----------/

    val graph = Set('a -> 'b11, 'b11 -> 'b12, 'b12 -> 'c, 'c -> 'd,
      'a -> 'b2, 'b2 -> 'c)
    val scores = Map ('a -> 2,
      'b11 -> 4, 'b12 -> 7,
      'b2 -> 6,
      'c -> 7, 'd -> 10)
    val n = new Network(graph, scores)

    val adjuster = new NetworkAdjuster
    val moves = adjuster.desiredMoves(n, Move('a, 6))
    
    moves should contain theSameElementsAs (List(
        Move('c, 9),
        Move('b12, 8),
        Move('b11, 7),
        Move('b2, 7),
        Move('a, 6)
    ))
  }
  
  "RippleAdjuster.solve" should "work for a simple linear graph" in {
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
  
  it should "work for a simple linear graph with a high end node" in {
    val graph = Set('a -> 'b, 'b -> 'c)
    val scores = Map ('a -> 1, 'b -> 2, 'c -> 6)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    val n2 = adjuster.solve(n, Move('a, 3))
    n2.scores('a) should equal (3)
    n2.scores('b) should equal (4)
    n2.scores('c) should equal (6)
  }
  
  it should "handle simple branching prerequisites" in {
    //      /-[b1 4]-[c1 10]
    // [a 2]
    //      \-[b2 6]-[c2 1]
    
    val graph = Set('a -> 'b1, 'b1 -> 'c1, 'a -> 'b2, 'b2 -> 'c2)
    val scores = Map ('a -> 2, 'b1 -> 4, 'b2 -> 6, 'c1 -> 10, 'c2 -> 12)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    val n2 = adjuster.solve(n, Move('a, 6))
    n2.scores('a) should equal (6)
    n2.scores('b1) should equal (7)
    n2.scores('b2) should equal (7)
    n2.scores('c1) should equal (10)
    n2.scores('c2) should equal (12)
  }
  
  it should "handle extended branching prerequisites" in {
    //      /-[b1 4]-[c1 7]-[d1 10]
    // [a 2]
    //      \-[b2 6]-[c2 8]-[d2 12]
    
    val graph = Set('a -> 'b1, 'b1 -> 'c1, 'c1 -> 'd1,
        'a -> 'b2, 'b2 -> 'c2, 'c2 -> 'd2)
    val scores = Map ('a -> 2,
        'b1 -> 4, 'b2 -> 6,
        'c1 -> 7, 'c2 -> 8,
        'd1 -> 10, 'd2 -> 12)
    val n = new Network(graph, scores)
    
    val adjuster = new NetworkAdjuster
    val n2 = adjuster.solve(n, Move('a, 7))
    n2.scores('a) should equal (7)
    n2.scores('b1) should equal (8)
    n2.scores('b2) should equal (8)
    n2.scores('c1) should equal (9)
    n2.scores('c2) should equal (9)
    n2.scores('d1) should equal (10)
    n2.scores('d2) should equal (12)
  }

  it should "ensure a partial move if just one branch prevents a full move" in {
    //      /-[b1 4]-[c1 7]-[d1 10]
    // [a 2]
    //      \-[b2 6]-[c2 7]-[d2 8]

    val graph = Set('a -> 'b1, 'b1 -> 'c1, 'c1 -> 'd1,
      'a -> 'b2, 'b2 -> 'c2, 'c2 -> 'd2)
    val scores = Map ('a -> 2,
      'b1 -> 4, 'b2 -> 6,
      'c1 -> 7, 'c2 -> 7,
      'd1 -> 10, 'd2 -> 8)
    val n = new Network(graph, scores)

    val adjuster = new NetworkAdjuster
    val n2 = adjuster.solve(n, Move('a, 7))
    n2.scores('a) should equal (5)
    n2.scores('b1) should equal (6)
    n2.scores('b2) should equal (6)
    n2.scores('c1) should equal (7)
    n2.scores('c2) should equal (7)
    n2.scores('d1) should equal (10)
    n2.scores('d2) should equal (8)
  }
  
  it should "work when branches to split and merge" in {
    //      /-[b1 4]-\
    // [a 2]          [c 7]-[d 9]
    //      \-[b2 6]-/

    val graph = Set('a -> 'b1, 'b1 -> 'c, 'c -> 'd,
      'a -> 'b2, 'b2 -> 'c)
    val scores = Map ('a -> 2,
      'b1 -> 4, 'b2 -> 6,
      'c -> 7,
      'd -> 9)
    val n = new Network(graph, scores)

    val adjuster = new NetworkAdjuster
    val n2 = adjuster.solve(n, Move('a, 6))
    n2.scores('a) should equal (6)
    n2.scores('b1) should equal (7)
    n2.scores('b2) should equal (7)
    n2.scores('c) should equal (8)
    n2.scores('d) should equal (9)
  }
  
  it should "work when merging branches make different demands on the merged branch (longer branch pushes more)" in {
    //      /-[b11 4]-[b12 7]-\
    // [a 2]                   [c 7]-[d 10]
    //      \-[b2 6]----------/

    val graph = Set('a -> 'b11, 'b11 -> 'b12, 'b12 -> 'c, 'c -> 'd,
      'a -> 'b2, 'b2 -> 'c)
    val scores = Map ('a -> 2,
      'b11 -> 4, 'b12 -> 7,
      'b2 -> 6,
      'c -> 7, 'd -> 10)
    val n = new Network(graph, scores)

    val adjuster = new NetworkAdjuster
    val n2 = adjuster.solve(n, Move('a, 6))
    n2.scores('a) should equal (6)
    n2.scores('b11) should equal (7)
    n2.scores('b12) should equal (8)
    n2.scores('b2) should equal (7)
    n2.scores('c) should equal (9)
    n2.scores('d) should equal (10)
  }
  
  it should "work when merging branches make different demands on the merged branch (shorter branch pushes more)" in {
    //      /-[b11 4]-[b12 5]-\
    // [a 2]                   [c 6]-[d 10]
    //      \-[b2_3 3]--------/                     <-- Note b2_3 is of size 2

    val graph = Set('a -> 'b11, 'b11 -> 'b12, 'b12 -> 'c, 'c -> 'd,
      'a -> 'b2_3, 'b2_3 -> 'c)
    val scores = Map ('a -> 2,
      'b11 -> 4, 'b12 -> 5,
      'b2_3 -> 3,
      'c -> 6, 'd -> 10)
    val n = new Network(graph, scores)

    val adjuster = new NetworkAdjuster
    val n2 = adjuster.solve(n, Move('a, 4))
    n2.scores('a) should equal (4)
    n2.scores('b11) should equal (5)
    n2.scores('b12) should equal (6)
    n2.scores('b2_3) should equal (5)
    n2.scores('c) should equal (8)
    n2.scores('d) should equal (10)
  }  
  
  it should "work when merging branches prevent any full move" in {
    //      /-[b11 4]-[b12 5]-\
    // [a 2]                   [c 6]
    //      \-[b2 5]----------/

    val graph = Set('a -> 'b11, 'b11 -> 'b12, 'b12 -> 'c,
      'a -> 'b2, 'b2 -> 'c)
    val scores = Map ('a -> 2,
      'b11 -> 4, 'b12 -> 5,
      'b2 -> 5,
      'c -> 6)
    val n = new Network(graph, scores)

    val adjuster = new NetworkAdjuster
    val n2 = adjuster.solve(n, Move('a, 5))
    n2.scores('a) should equal (3)
    n2.scores('b11) should equal (4)
    n2.scores('b12) should equal (5)
    n2.scores('b2) should equal (5)
    n2.scores('c) should equal (6)
  }  
    
  "RippleAdjuster.resolve" should "simply add two lists moves on different pieces" in {
    val adjuster = new NetworkAdjuster
    val moves = adjuster.combine(List(Move('a, 1), Move('b, 2)), List(Move('c, 3), Move('d, 4)))
    moves should equal (List(Move('a, 1), Move('b, 2), Move('c, 3), Move('d, 4)))
  }
  
  it should "drop a move in ms2 if it also appears in ms1" in {
    val adjuster = new NetworkAdjuster
    val moves = adjuster.combine(List(Move('a, 1), Move('b, 2)), List(Move('a, 1), Move('d, 4)))
    moves should equal (List(Move('a, 1), Move('b, 2), Move('d, 4)))
  }
  
  it should "drop a move in ms2 if the piece is moved in ms1" in {
    val adjuster = new NetworkAdjuster
    val moves = adjuster.combine(List(Move('a, 10), Move('b, 2)), List(Move('a, 1), Move('d, 4)))
    moves should equal (List(Move('a, 10), Move('b, 2), Move('d, 4)))
  }
  
  it should "replace a move in ms1 if the piece is moved further in ms2" in {
    val adjuster = new NetworkAdjuster
    val moves = adjuster.combine(List(Move('a, 1), Move('b, 2)), List(Move('a, 10), Move('d, 4)))
    moves should equal (List(Move('a, 10), Move('b, 2), Move('d, 4)))
  }
  
  it should "dedupe moves from the first list if they're identical" in {
    val adjuster = new NetworkAdjuster
    val moves = adjuster.combine(List(Move('a, 1), Move('a, 1)), List(Move('c, 3), Move('d, 4)))
    moves should equal (List(Move('a, 1), Move('c, 3), Move('d, 4)))
  }
  
  it should "dedupe non-maximal moves from the first list (1)" in {
    val adjuster = new NetworkAdjuster
    val moves = adjuster.combine(List(Move('a, 10), Move('a, 1)), List(Move('c, 3), Move('d, 4)))
    moves should equal (List(Move('a, 10), Move('c, 3), Move('d, 4)))
  }
  
  it should "dedupe non-maximal moves from the first list (2 - other way round)" in {
    val adjuster = new NetworkAdjuster
    val moves = adjuster.combine(List(Move('a, 1), Move('a, 10)), List(Move('c, 3), Move('d, 4)))
    moves should equal (List(Move('a, 10), Move('c, 3), Move('d, 4)))
  }

}