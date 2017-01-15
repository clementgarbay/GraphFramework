package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
trait IDirectedGraph extends IGraph {

  /**
    * The number of arcs in the graph
    */
  val nbArcs: Int

  /**
    * The inverse graph
    */
  val computeInverse: IDirectedGraph

  /**
    * The corresponding undirected graph from the directed graph
    */
  val toUndirectedGraph: IUndirectedGraph

  /**
    * @param from
    * @param to
    * @return True iff arc (from,to) figures in the graph
    */
  def isArc(from: Int, to: Int): Boolean

  /**
    * Adds the arc (from,to) if it is not already present in the graph, requires from /= to
    * @param from
    * @param to
    */
  def addArc(from: Int, to: Int): IDirectedGraph

  /**
    * The arc (from,to), if exists
    * @param from
    * @param to
    */
  def removeArc(from: Int, to: Int): IDirectedGraph

  /**
    * @param node
    * @return A int list representing successors of node
    */
  def getSuccessors(node: Int): Set[Int]

  /**
    * @param node
    * @return A int list representing predecessors of node
    */
  def getPredecessors(node: Int): Set[Int]

}
