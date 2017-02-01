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
    * Return successors or neighbors depending on the type of graph
    */
  override def getNextNodes(nodeId: T): Set[SemiArc[T]] = {
    getSuccessors(nodeId)
  }

  /**
    * Return ids of successors or neighbors depending on the type of graph
    */
  override def getNextNodesId(nodeId: T): Set[T] = {
    getSuccessorsIds(nodeId)
  }

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
    * Compute shortest distances from a node to all other node in the graph.
    * Using Bellman-Ford algorithm.
    *
    * TODO: use a `Path` object to store parents ?
    * TODO: add tests
    *
    * @param startingNodeId The starting node id
    * @return               (distances, parents)
    */
  def getShortestPathWithBellmanFord(startingNodeId: T): (Map[T, Double], List[Path[T]]) = {
    var distances: Map[T, Double] = nodesIds.map(nodeId => nodeId -> Double.MaxValue).toMap
    var parents: Map[T, Option[SemiArc[T]]] = nodesIds.map(nodeId => nodeId -> Option.empty).toMap

    distances = distances + (startingNodeId -> 0.0)

    for {
      _ <- 1 until nbNodes - 1
      arc <- arcs if arc.to != startingNodeId && distances(arc.to) > distances(arc.from) + arc.distance
    } {
      distances = distances + (arc.to -> (distances(arc.from) + arc.distance))
      parents = parents + (arc.to -> Some(SemiArc(arc.from, getDistance(arc.from, arc.to))))
    }

    val paths = nodesIds
      .filter(_ != startingNodeId)
      .map(nodeId => Path.getPathFromParents(startingNodeId, nodeId, parents))

    (distances, paths)
  }
}
