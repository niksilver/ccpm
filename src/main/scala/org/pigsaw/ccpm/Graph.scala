package org.pigsaw.ccpm

import scala.annotation.tailrec

/**
 * An acyclic graph.
 */
class Graph[T](g: Set[(T, T)]) {

  /**
   * Get all successor nodes available from a given node
   */
  def successors(node: T): Set[T] = g filter { _._1 == node } map { _._2 }
  
  /**
   * Get the predecessors of a given node: i.e. the nodes with an edge
   * that go into the given one.
   */
  def predecessors(node: T): Set[T] = g filter { _._2 == node } map { _._1 }

  /**
   * Does the graph have a given edge?
   */
  def hasEdge(e: Tuple2[T, T]): Boolean = { g contains e }
  
  /**
   * Does the graph have the given node?
   */
  def hasNode(node: T): Boolean = {
    g exists { pair => node == pair._1 || node == pair._2 }
  }

  /**
   * Given an acyclic graph `g`, does it remain acyclic
   * if we add the specified `edge`?
   */
  def remainsAcyclic(edge: (T, T)): Boolean = {

    val from = edge._1
    val to = edge._2

    if (from == to)
      return false

    // If adding this edge would make the graph cyclic then
    // that's because we can already get from `to` to `from`
    // via some depth-first traversal

    val end = from
    val begin = to

    def canReachEnd(start: T): Boolean = {
      val nexts = successors(start)
      nexts.contains(end) || (nexts exists { canReachEnd(_) })
    }

    val isCyclic = canReachEnd(begin)
    !isCyclic
  }

  /**
   * Return the nodes which are at the end of a path.
   */
  def ends: Set[T] = {
    val froms = g map { _._1 }
    val tos = (g map { _._2 })
    tos diff froms
  }

  /**
   * Return the nodes which are at the start of a path.
   */
  def starts: Set[T] = {
    val froms = (g map { _._1 })
    val tos = g map { _._2 }
    froms diff tos
  }

  /**
   * Get all the paths through this acyclic graph.
   */
  def paths: Set[Seq[T]] = pathsFrom(starts)

  // Get all the paths from (and including) the given nodes.
  private def pathsFrom(source: Set[T]): Set[Seq[T]] = {
    for {
      next <- source
      path <- pathsAfter(next)
    } yield (next +: path)
  }

  // Get all the paths after (and therefore excluding) the given node.
  private def pathsAfter(source: T): Set[Seq[T]] = {
    val succs = successors(source)
    if (succs.isEmpty) {
      Set(Nil)
    } else {
      pathsFrom(succs)
    }
  }
  
  /**
   * Get all paths for which the given node is the last node.
   */
  def pathsTo(n: T): Set[Seq[T]] = {
    pathsTo(Set(n))
  }
  
  /**
   * Get all the paths for which any of the given nodes is the last node.
   */
  def pathsTo(dests: Set[T]): Set[Seq[T]] = {
    val noNodes = { node: T => false }
    pathsTo(dests, noNodes)
  }
  
  /**
   * Get all the paths for which any of the given nodes is the last node,
   * and treating some nodes as if they weren't in the graph at all.
   * @param dests  Each path must terminate at one these nodes.
   * @param excluded  A function which allows us to treat a node as
   *     if it weren't in the graph. If `excluded(n)` is `true` then
   *     we pretend `n` has been removed from the graph.
   */
  def pathsTo(dests: Set[T], excluded: T => Boolean): Set[Seq[T]] = {
    for {
      dest <- dests
      path <- pathsBefore(dest, excluded)
    } yield (path :+ dest)
  }
  
  // Get all the paths before (and therefore excluding) the given node
  private def pathsBefore(dest: T, excluded: T => Boolean): Set[Seq[T]] = {
    val preds = predecessors(dest) filterNot excluded
    if (preds.isEmpty) {
      Set(Nil)
    } else {
      pathsTo(preds, excluded)
    }
  }

}