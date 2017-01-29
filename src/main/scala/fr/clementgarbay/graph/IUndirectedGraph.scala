package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
trait IUndirectedGraph[T] extends IGraph[T] {

  /**
    * A list of edges with starting node, ending node and the distance between both
    */
  val edges: List[Edge[T]]

  /**
    * The number of edges in the graph
    */
  val nbEdges: Int

  /**
    * Check the connectivity of the graph
    */
  val isConnected: Boolean =
    if (nodesIds.isEmpty) false
    else depthFirstSearch(nodesIds.head).size == nbNodes

  /**
    * Tests if two nodes are a edge
    *
    * @param from The id of the first node
    * @param to   The id of the second node
    * @return     True if there is an edge between x and y
    */
  def isEdge(from: T, to: T): Boolean

  /**
    * Adds edge (from,to) if not already present, requires from /= to
    *
    * @param from The id of the first node
    * @param to   The id of the second node
    * @param distance The distance between the two nodes
    * @return     A new graph with the new edge
    */
  def addEdge(from: T, to: T, distance: Double): IUndirectedGraph[T]

  /**
    * @see addEdge(from: T, to: T, distance: Double)
    */
  def addEdge(from: T, to: T): IUndirectedGraph[T]

  /**
    * Removes edge (from,to) if exists
    *
    * @param from The id of the first node
    * @param to   The id of the second node
    * @return     A new graph with modification
    */
  def removeEdge(from: T, to: T): IUndirectedGraph[T]

  /**
    * Get neighbors of a specific node
    *
    * @param nodeId The related node id
    * @return       A list of tuple representing neighbors ids with distances from the node
    */
  def getNeighbors(nodeId: T): Set[SemiEdge[T]]

  /**
    * Get neighbors of a specific node
    *
    * @param nodeId The related node id
    * @return       A list representing neighbors ids of node
    */
  def getNeighborsIds(nodeId: T): Set[T]

  /**
    * Explore a graph with the depth first search algorithm
    *
    * @param startingNodeId The starting node id
    * @return               All nodes ids reachable from the starting node
    */
  def depthFirstSearch(startingNodeId: T): Set[T] = {
    def depthFirstSearchRec(node: T, visited: Set[T]): Set[T] = {
      if (visited contains node) visited
      else getNeighborsIds(node).foldLeft(visited + node)((visitedNodes, neighbor) => depthFirstSearchRec(neighbor, visitedNodes))
    }

    depthFirstSearchRec(startingNodeId, Set())
  }

  /**
    * Explore a graph with the breadth first search algorithm
    *
    * @param startingNodeId The starting node id
    * @return               All nodes ids reachable from the starting node
    */
  def breadthFirstSearch(startingNodeId: T): Set[T] = {
    def breadthFirstSearchRec(toVisit: Set[T], visited: Set[T]): Set[T] = {
      val neighbors = toVisit.flatMap(getNeighborsIds).diff(visited)
      if (neighbors.isEmpty) visited
      else breadthFirstSearchRec(neighbors, visited ++ neighbors)
    }

    breadthFirstSearchRec(Set(startingNodeId), Set(startingNodeId))
  }

}
