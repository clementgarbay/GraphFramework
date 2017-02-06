package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  *
  * @tparam T The type of the node id
  */
trait IDirectedGraph[T] extends IGraph[T, SemiArc[T]] {

  /**
    * A list of arcs with starting node, ending node and the distance between both
    */
  val arcs: List[Arc[T]]

  /**
    * The number of arcs in the graph
    */
  val nbArcs: Int

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
    * Get the distance between two nodes
    *
    * @param from The id of the first node
    * @param to   The id of the second node
    * @return
    */
  def getDistance(from: T, to: T): Double

  /**
    * Get successors of a specific node
    *
    * @param nodeId The related node id
    * @return       A list of tuple representing successors ids with distances from the node
    */
  def getSuccessors(nodeId: T): Set[SemiArc[T]]

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
  def getPredecessors(nodeId: T): Set[SemiArc[T]]

  /**
    * Get predecessors of a specific node
    *
    * @param nodeId The related node id
    * @return       A list representing predecessors ids of the node
    */
  def getPredecessorsIds(nodeId: T): Set[T]

  /**
    * @inheritdoc
    */
  override def getNextNodes(nodeId: T): Set[SemiArc[T]] = getSuccessors(nodeId)

  /**
    * @inheritdoc
    */
  override def getNextNodesId(nodeId: T): Set[T] = getSuccessorsIds(nodeId)

  /**
    * Compute shortest distances from a node to all other nodes in the graph.
    * Using Bellman-Ford algorithm.
    *
    * @param startingNodeId The starting node id
    * @return               The different paths computed to all other nodes
    */
//  def getShortestPathWithBellmanFord(startingNodeId: T): List[Path[T]] = {
//    var candidatePaths: Map[T, Path[T]] = nodesIds.map(nodeId => nodeId -> Path(Double.MaxValue, List(startingNodeId))).toMap
//
//    candidatePaths = candidatePaths + (startingNodeId -> Path(0.0, List(startingNodeId)))
//
//    for {
//      _ <- 1 until nbNodes - 1
//      arc <- arcs
//      if arc.to != startingNodeId && candidatePaths(arc.to).distance > candidatePaths(arc.from).distance + arc.distance
//    } {
//      candidatePaths = candidatePaths + (arc.to -> candidatePaths(arc.from).addToTop(arc.distance, arc.to))
//    }
//
//    candidatePaths
//      .filter(_._1 != startingNodeId)
//      .map(_._2.reverse).toList
//  }

  def getShortestPathWithBellmanFord(startingNodeId: T): (Map[T, Double], Map[T, Option[T]]) = {
    var distances: Map[T, Double] = nodesIds.map(nodeId => nodeId -> Double.MaxValue).toMap
    var parents: Map[T, Option[T]] = nodesIds.map(nodeId => nodeId -> Option.empty).toMap

    distances = distances + (startingNodeId -> 0.0)

    for {
      _ <- 1 until nbNodes - 1
      arc <- arcs
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
    def getShortestPathWithDijkstraRec(candidatePaths: List[Path[T]], visited: Set[T]): Path[T] =
      candidatePaths match {
        case candidatePath :: candidatePaths_rest => candidatePath.path match {
          case firstInPath :: _ =>
            if (firstInPath == endingNodeId) candidatePath.reverse
            else {
              val newCandidatePaths = getSuccessors(firstInPath)
                .toList
                .collect {
                  case (successor: SemiArc[T]) if !visited.contains(successor.to) =>
                    candidatePath.addToTop(successor.distance, successor.to)
                }

              val newCandidatesSorted = (newCandidatePaths ++ candidatePaths_rest).sortWith {
                case (path1, path2) => path1.distance < path2.distance
              }

              getShortestPathWithDijkstraRec(newCandidatesSorted, visited + firstInPath)
            }
          case _ => Path.empty
        }
        case _ => Path.empty
      }

    getShortestPathWithDijkstraRec(List(Path(0.0, List(startingNodeId))), Set())
  }
}
