package fr.clementgarbay.graph

import fr.clementgarbay.graph.heap.BinaryHeap

import scala.collection.immutable.ListMap
import scala.collection.mutable

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
    * Give the distance of a graph
    */
  lazy val diameter: Double = {
    nodesIds.map(getShortestPathWithBellmanFord).map(_.maxBy(_.distance)).maxBy(_.distance).distance
  }

  /**
    * Return the nodes of the graph
    */
  def getNodes: List[T]

  /**
    * Return the links of the graph
    */
  def getLinks: List[Link[T]]

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
    * Simple BFS traversal
    */
  def bfs(root: T): List[T] = {
    val q = new mutable.Queue[T]()
    var node = root
    var result: List[T] = List.empty

    q.enqueue(node)
    while (q.nonEmpty) {
      node = q.dequeue()
      result = result :+ node
      getNextNodes(node).foreach(e => if (!result.contains(e.to) && !q.contains(e.to)) q.enqueue(e.to))
      print(node + " ")
    }

    result
  }

  def naiveDiameter = {
    /**
      * Simple BFS traversal
      */
    def getHeight(root: T): Int = {
      val q = new mutable.Queue[T]()
      var node = root
      var result: List[T] = List.empty

      var dist: Map[T, Int] = getNodes.map(e => e -> 0).toMap

      q.enqueue(node)
      while (q.nonEmpty) {
        node = q.dequeue()
        result = result :+ node
        getNextNodes(node).foreach(e => {
          if (!result.contains(e.to) && !q.contains(e.to)) {
            dist = dist.updated(e.to, dist(node) + 1)
            q.enqueue(e.to)
          }
        })
      }

      dist.maxBy(_._2)._2
    }

    getNodes.map(e => getHeight(e)).max
  }

  /**
    * Simple DFS traversal
    */
  def dfs(root: T): List[T] = {
    val s = new mutable.Stack[T]()
    var node = root
    var result: List[T] = List.empty

    s.push(node)
    while (s.nonEmpty) {
      node = s.pop()
      result = result :+ node
      getNextNodes(node).foreach(e => if (!result.contains(e.to) && !s.contains(e.to)) s.push(e.to))
      print(node + " ")
    }

    result
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

  /********
    * AS WE WORK ON SIMPLE GRAPH THESE ALGORITHME WORK ON DIRECTED AND UNDIRECTED GRAPHS
    *******/

  /**
    * Compute shortest distances from a node to all other nodes in the graph.
    * Using Bellman-Ford algorithm.
    *
    * @param startingNodeId The starting node id
    * @return               The different paths computed to all other nodes
    */
  def getShortestPathWithBellmanFord(startingNodeId: T): List[Path[T]] = {
    var candidatePaths: Map[T, Path[T]] = nodesIds.map(nodeId => nodeId -> Path(Double.MaxValue, List(startingNodeId))).toMap

    candidatePaths = candidatePaths + (startingNodeId -> Path(0.0, List(startingNodeId)))

    for {
      _ <- 1 until nbNodes - 1
      arc <- getLinks
      if arc.to != startingNodeId && candidatePaths(arc.to).distance > candidatePaths(arc.from).distance + arc.distance
    } {
      candidatePaths = candidatePaths + (arc.to -> candidatePaths(arc.from).addToTop(arc.distance, arc.to))
    }

    candidatePaths
      .filter(_._1 != startingNodeId)
      .map(_._2.reverse).toList
  }

  def getShortestPathWithBellmanFord_EASY(startingNodeId: T): (Map[T, Double], Map[T, Option[T]]) = {
    var distances: Map[T, Double] = nodesIds.map(nodeId => nodeId -> Double.MaxValue).toMap
    var parents: Map[T, Option[T]] = nodesIds.map(nodeId => nodeId -> Option.empty).toMap

    distances = distances + (startingNodeId -> 0.0)

    for {
      _ <- 1 until nbNodes - 1
      arc <- getLinks
      if arc.to != startingNodeId && distances(arc.to) > distances(arc.from) + arc.distance
    } {
      distances = distances + (arc.to -> (distances(arc.from) + arc.distance))
      parents = parents + (arc.to -> Some(arc.from))
    }

    (distances, parents)
  }

  /**
    * Compute shortest path between two nodes in the graph.
    * Using Dijkstra algorithm.
    *
    * @param startingNodeId The starting node id
    * @param endingNodeId   The ending node id
    * @return               The path with the smallest distance
    */
  def getShortestPathWithDijkstra(startingNodeId: T, endingNodeId: T): Path[T] = {

    def computeNewCandidatePaths(candidatePath: Path[T], visited: Set[T], otherPaths: List[Path[T]]): List[Path[T]] = {
      val lastInPath = candidatePath.path.head
      // For each successors of the last node in this candidate path
      val newCandidatePaths = getNextLinks(lastInPath)
        .toList
        .collect {
          // Construct new paths by adding current successor to this candidate path, if the current successor is not visited
          case (successor: Link[T]) if !visited.contains(successor.to) =>
            candidatePath.addToTop(successor.distance, successor.to)
        }

      // Sort candidate paths by distance
      (newCandidatePaths ++ otherPaths).sortWith {
        case (path1, path2) => path1.distance < path2.distance
      }
    }

    def getShortestPathWithDijkstraRec(candidatePaths: List[Path[T]], visited: Set[T]): Path[T] =
      candidatePaths match {
        // Take the candidate path with the shortest distance
        case candidatePath :: candidatePaths_rest => candidatePath.path match {
          // Take the last node in the path
          case lastInPath :: _ =>
            // If the last node in the path is the desired ending node, return the current candidate path
            if (lastInPath == endingNodeId) candidatePath.reverse
            // Otherwise, re-call the getShortestPathWithDijkstraRec method with the new candidate paths list computed
            else {
              val newCandidatePaths = computeNewCandidatePaths(candidatePath, visited, candidatePaths_rest)
              getShortestPathWithDijkstraRec(newCandidatePaths, visited + lastInPath)
            }
          case _ => Path.empty
        }
        case _ => Path.empty
      }

    getShortestPathWithDijkstraRec(List(Path(0.0, List(startingNodeId))), Set())
  }
}
