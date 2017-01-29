package fr.clementgarbay.graph

import scala.collection.immutable.ListMap

/**
  * @author Clément Garbay
  *
  * @tparam T The type of the node id
  */
trait IDirectedGraph[T] extends IGraph[T] {

  /**
    * A list of tuple with starting node, ending node and the distance between both
    */
  val arcs: List[(T, T, Double)]

  /**
    * The number of arcs in the graph
    */
  val nbArcs: Int

  /**
    * The inverse graph
    */
  val inverse: IDirectedGraph[T]

  /**
    * The corresponding undirected graph from the directed graph
    */
  val toUndirectedGraph: IUndirectedGraph[T]

  /**
    * Check the strong connectivity of the graph
    */
  lazy val isConnected: Boolean =
    if (nodesIds.isEmpty) false
    else depthFirstSearch(nodesIds.head).size == nbNodes && inverse.depthFirstSearch(nodesIds.head).size == nbNodes

  /**
    * Tests if two nodes are an arc
    *
    * @param from The id of the first node
    * @param to   The id of the second node
    * @return     True iff arc (from,to) figures in the graph
    */
  def isArc(from: T, to: T): Boolean

  /**
    * Adds the arc (from,to) if it is not already present in the graph, requires from /= to
    *
    * @param from     The first node
    * @param to       The second node
    * @param distance The distance between the two nodes
    * @return         A new graph with the new arc
    */
  def addArc(from: T, to: T, distance: Double): IDirectedGraph[T]

  /**
    * @see addArc(from: T, to: T, distance: Double)
    */
  def addArc(from: T, to: T): IDirectedGraph[T]

  /**
    * Remove the arc (from,to), if exists
    *
    * @param from The first node
    * @param to   The second node
    * @return     A new graph with modification
    */
  def removeArc(from: T, to: T): IDirectedGraph[T]

  /**
    * Get successors of a specific node
    *
    * @param nodeId The related node id
    * @return       A list of tuple representing successors ids with distances from the node
    */
  def getSuccessors(nodeId: T): Set[(T, Double)]

  /**
    * Get successors of a specific node
    *
    * @param nodeId The related node id
    * @return       A list representing successors ids of the node
    */
  def getSuccessorsIds(nodeId: T): Set[T]

  /**
    * Get predecessors of a specific node
    *
    * @param nodeId The related node id
    * @return       A list of tuple representing predecessors ids with distances to the node
    */
  def getPredecessors(nodeId: T): Set[(T, Double)]

  /**
    * Get predecessors of a specific node
    *
    * @param nodeId The related node id
    * @return       A list representing predecessors ids of the node
    */
  def getPredecessorsIds(nodeId: T): Set[T]

  /**
    * Explore a graph with the depth first search algorithm
    *
    * @param startingNodeId The starting node id
    * @return               All nodes ids reachable from the starting node
    */
  def depthFirstSearch(startingNodeId: T): Set[T] = {
    def depthFirstSearchRec(node: T, visited: Set[T]): Set[T] = {
      if (visited contains node) visited
      else getSuccessorsIds(node).foldLeft(visited + node)((visitedNodes, successor) => depthFirstSearchRec(successor, visitedNodes))
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
      val successors = toVisit.flatMap(getSuccessorsIds).diff(visited)
      if (successors.isEmpty) visited
      else breadthFirstSearchRec(successors, visited ++ successors)
    }

    breadthFirstSearchRec(Set(startingNodeId), Set(startingNodeId))
  }

  /**
    * Compute shortest distances from a node to all other node in the graph.
    * Using Bellman-Ford algorithm.
    *
    * TODO: use a `Path` object to store parents ?
    * TODO: add tests
    *
    * @param startingNodeId The starting node id
    * @return               (distances, parents)
    */
  def getShortestPathWithBellmanFord(startingNodeId: T): (Map[T, Double], Map[T, Option[T]]) = {
    var distances: Map[T, Double] = nodesIds.map(nodeId => nodeId -> Double.MaxValue).toMap
    var parents: Map[T, Option[T]] = nodesIds.map(nodeId => nodeId -> Option.empty).toMap

    distances = distances + (startingNodeId -> 0.0)

    for {
      _ <- 1 until nbNodes - 1
      (n1, n2, distance) <- arcs if n2 != startingNodeId && distances(n2) > distances(n1) + distance
    } {
      distances = distances + (n2 -> (distances(n1) + distance))
      parents = parents + (n2 -> Some(n1))
    }

    (distances, parents)
  }

  /**
    * Get all strongly connected components from a node id
    *
    * @param startingNodeId The starting node id
    * @return
    */
  def getStronglyConnectedComponents(startingNodeId: T): List[List[T]] = {
    val sortedEnd = ListMap(depthFirstSearchWithInfo(startingNodeId)._2.toSeq.sortWith(_._2 > _._2):_*)
    val scc = inverse.depthFirstSearchWithInfo(sortedEnd.head._1, sortedEnd.keys.toList)
    scc._3
  }

  /**
    * GO AWAY VERY UGLY METHOD
    *
    * TODO: refactor
    *
    * @param startingNodeId The starting node id
    * @param listToFollow   A list of nodes ids to follow
    * @return               (start, end, visited)
    */
  private def depthFirstSearchWithInfo(startingNodeId: T, listToFollow: List[T] = nodesIds): (Map[T, Int], Map[T, Int], List[List[T]]) = {
    var increment: Int = 0
    var start: Map[T, Int] = Map.empty
    var end: Map[T, Int] = Map.empty

    def depthFirstSearchRec(node: T, visited: List[T]): List[T] = {
      increment += 1
      start += node -> increment

      val currentVisit = getSuccessorsIds(node)
        .toList
        .diff(visited :+ node)
        .foldLeft(visited :+ node)((acc, successor) => acc ++ depthFirstSearchRec(successor, acc))

      increment += 1
      end += node -> increment

      currentVisit
    }

    var visited: List[List[T]] = List(depthFirstSearchRec(startingNodeId, List.empty))

    visited = listToFollow
      .diff(visited.flatten)
      .filter(node => !(visited.flatten contains node))
      .foldLeft(visited)((acc, node) => acc :+ depthFirstSearchRec(node, acc.flatten).diff(acc.flatten))

    (start, end, visited)
  }

}
