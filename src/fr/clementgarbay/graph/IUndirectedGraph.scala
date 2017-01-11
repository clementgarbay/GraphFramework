package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
trait IUndirectedGraph extends IGraph {

  /**
    * @return The number of edges in the graph
    */
  def getNbEdges: Int

  /**
    * @param x
    * @param y
    * @return True if there is an edge between x and y
    */
  def isEdge(x: Int, y: Int): Boolean

  /**
    * Removes edge (x,y) if exists
    * @param x
    * @param y
    * @return
    */
  def removeEdge(x: Int, y: Int): AdjacencyListUndirectedGraph

  /**
    * Adds edge (x,y) if not already present, requires x /= y
    * @param x
    * @param y
    */
  def addEdge(x: Int, y: Int): AdjacencyListUndirectedGraph

  /**
    * @param x
    * @return A new int list representing neighbors of node x
    */
  def getNeighbors(x: Int): List[Int]
}
