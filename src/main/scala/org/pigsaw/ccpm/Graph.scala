package org.pigsaw.ccpm

/**
 * An acyclic graph.
 */
class Graph[T](g: Seq[(T,T)]) {
  
  /**
   * Given an acyclic graph `g`, does it remain acyclic
   * if we add the specified `edge`?
   */
  def remainsAcyclic(edge: (T,T)): Boolean = {
      
    val from = edge._1
    val to = edge._2

    if (from == to)
      return false
    
    // If adding this edge would make the graph cyclic then
    // that's because we can already get from `to` to `from`
    // via some depth-first traversal
    
    val end = from
    val begin = to
    
    // Get all each target node available from a given node
    def targets(node: T): Seq[T] = g filter { _._1 == node } map { _._2 }
    
    def canReachEnd(start: T): Boolean = {
      val nexts = targets(start)
      nexts.contains(end) || (nexts exists { canReachEnd(_) })
    }
    
    val isCyclic = canReachEnd(begin)
    !isCyclic
  }
  
  /**
   * Return the nodes which are at the end of a path.
   */
  def ends: Seq[T] = {
    val froms = (g map { _._1 })
    val tos = (g map { _._2 }).distinct
    tos diff froms
  }
}