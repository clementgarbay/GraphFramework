package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
trait IGraph {

  /**
    * @return The number of nodes in the graph (referred to as the order of the graph)
    */
  def getNbNodes: Int

  /**
    * @return The adjacency matrix representation of the graph
    */
  def toAdjacencyMatrix: List[List[Int]]
}
