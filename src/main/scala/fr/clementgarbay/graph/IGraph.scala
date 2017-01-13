package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
trait IGraph {

  /**
    * The number of nodes in the graph (referred to as the order of the graph)
    */
  val nbNodes: Int

  /**
    * @return The adjacency matrix representation of the graph
    */
  def toAdjacencyMatrix: List[List[Int]]
}
