package fr.clementgarbay.graph

import fr.clementgarbay.graph.heap.BinaryHeap

import scala.collection.immutable.ListMap

/**
  * @author Clément Garbay
  * @author Anaël Chardan
  *
  * @tparam T The type of the node id
  * @tparam U SemiEdge or SemiArc
  */
trait IGraph[T, U <: SemiLinkTransformable[T]] {

  /**
    * The number of nodes in the graph (referred to as the order of the graph)
    */
  val nbNodes: Int

  /**
    * The associated adjacency matrix representation of the graph
    */
  val toAdjacencyMatrix: Map[T, Map[T, Int]]

  /**
    * Return the list of existing nodes ids in the graph
    */
  val nodesIds: List[T]

  /**
    * Return the inverse of the graph
    */
  val inverse: IGraph[T, U]

  /**
    * Check if the graph is connected
    */
  val isConnected: Boolean

  /**
    * Check if the graph is drawable with one trait
    */
  val admitEulerianChainOrPath: Boolean

  /**
    * Check if the graph is eulerian
    */
  val isEulerian: Boolean

  /**
    * Return the nodes of the graph
    */
  def getNodes: List[T]

  /**
    * Return successors or neighbors depending on the type of graph
    */
  def getNextNodes(nodeId: T): Set[U]

  /**
    * Return Links from a node
    */
  def getNextLinks(nodeId: T): Set[Link[T]] = {
    getNextNodes(nodeId).map(e => e.toLink(nodeId))
  }

  /**
    * Return ids of successors or neighbors depending on the type of graph
    */
  def getNextNodesId(nodeId: T): Set[T]

  /**
    * Explore a graph with the depth first search algorithm
    *
    * @param startingNodeId The starting node id
    * @return               All nodes ids reachable from the starting node
    */
  def depthFirstSearch(startingNodeId: T): Set[T] = {
    def depthFirstSearchRec(node: T, visited: Set[T]): Set[T] = {
      if (visited contains node) visited
      else getNextNodesId(node).foldLeft(visited + node)((visitedNodes, successor) => depthFirstSearchRec(successor, visitedNodes))
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
      val successors = toVisit.flatMap(getNextNodesId).diff(visited)
      if (successors.isEmpty) visited
      else breadthFirstSearchRec(successors, visited ++ successors)
    }

    breadthFirstSearchRec(Set(startingNodeId), Set(startingNodeId))
  }

  /**
    * Get all strongly connected components from a node id
    *
    * @param startingNodeId The starting node id
    * @return
    */
  def getStronglyConnectedComponents(startingNodeId: T): Set[Set[T]] = {
    val sortedEnd = ListMap(depthFirstSearchWithInfo(startingNodeId)._2.toSeq.sortWith(_._2 > _._2):_*)
    val scc = inverse.depthFirstSearchWithInfo(sortedEnd.head._1, sortedEnd.keys.toList)
    scc._3
  }

  /**
    * Get the Minimum Spanning Tree using Prim algorithm
    *
    * @param startingNodeId the starting node id
    * @return
    */
  def prim(startingNodeId: T): Set[Link[T]] = {
    def addNodesInHeap(node: T, currentHeap: BinaryHeap[Link[T]]): BinaryHeap[Link[T]] = {
      getNextLinks(node).foldLeft(currentHeap){(acc: BinaryHeap[Link[T]], i: Link[T]) => acc.add(i)}
    }

    def filterRoot(toRemove: Set[T], heap: BinaryHeap[Link[T]]): BinaryHeap[Link[T]] = {
      Stream.iterate(heap)(_.removeRoot()).dropWhile(e => e.root match {
        case Some(link: Link[T]) => toRemove.contains(link.to)
        case None => false
      }).head
    }

    def primRec(visited: Set[T], currentBinaryHeap: BinaryHeap[Link[T]], currentHeap: Set[Link[T]] = Set.empty): Set[Link[T]] = {
      filterRoot(visited, currentBinaryHeap).root match {
        case Some(minimalLink: Link[T]) =>
          primRec(visited + minimalLink.to, addNodesInHeap(minimalLink.to, currentBinaryHeap), currentHeap + minimalLink)
        case None => currentHeap
      }
    }

    primRec(Set(startingNodeId), addNodesInHeap(startingNodeId, BinaryHeap[Link[T]](List.empty)))
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
  private def depthFirstSearchWithInfo(startingNodeId: T, listToFollow: List[T] = nodesIds): (Map[T, Int], Map[T, Int], Set[Set[T]]) = {
    var increment: Int = 0
    var start: Map[T, Int] = Map.empty
    var end: Map[T, Int] = Map.empty

    def depthFirstSearchRec(node: T, visited: Set[T]): Set[T] = {
      increment += 1
      start += node -> increment

      val currentVisit = getNextNodesId(node)
        .diff(visited + node)
        .foldLeft(visited + node)((acc, successor) => acc ++ depthFirstSearchRec(successor, acc))

      increment += 1
      end += node -> increment

      currentVisit
    }

    var visited: Set[Set[T]] = Set(depthFirstSearchRec(startingNodeId, Set.empty))

    visited = listToFollow
      .diff(visited.toList.flatten)
      .filter(node => !(visited.flatten contains node))
      .foldLeft(visited)((acc, node) => {
        val res = depthFirstSearchRec(node, acc.flatten).diff(acc.flatten)
        if (res.nonEmpty) acc + res else acc
      })

    (start, end, visited)
  }

}
