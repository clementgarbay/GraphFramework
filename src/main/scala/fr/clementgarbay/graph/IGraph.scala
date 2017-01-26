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
    * The associated adjacency matrix representation of the graph
    */
  val toAdjacencyMatrix: List[List[Int]]

  /**
    * Return the list of existing nodes in the graph
    */
  val nodesList: Set[Int]
}
