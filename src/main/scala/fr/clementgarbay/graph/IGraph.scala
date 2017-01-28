package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  *
  * @tparam T The type of the node id
  */
trait IGraph[T] {

  /**
    * The number of nodes in the graph (referred to as the order of the graph)
    */
  val nbNodes: Int

  /**
    * The associated adjacency matrix representation of the graph
    */
  val toAdjacencyMatrix: List[List[Int]]

  /**
    * Return the list of existing nodes ids in the graph
    */
  val nodesIds: Set[T]
}
