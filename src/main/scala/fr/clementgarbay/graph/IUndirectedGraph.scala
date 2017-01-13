package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
trait IUndirectedGraph extends IGraph {

  /**
    * The number of edges in the graph
    */
  val nbEdges: Int

  /**
    * @param from
    * @param to
    * @return True if there is an edge between x and y
    */
  def isEdge(from: Int, to: Int): Boolean

  /**
    * Adds edge (from,to) if not already present, requires from /= to
    * @param from
    * @param to
    */
  def addEdge(from: Int, to: Int): IUndirectedGraph

  /**
    * Removes edge (from,to) if exists
    * @param from
    * @param to
    * @return
    */
  def removeEdge(from: Int, to: Int): IUndirectedGraph

  /**
    * @param node
    * @return A int list representing neighbors of node
    */
  def getNeighbors(node: Int): Set[Int]
}
