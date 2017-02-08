package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  * @author Anaël Chardan
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
    * Check the connectivity of a graph
    */
  override lazy val isConnected: Boolean = toUndirectedGraph.isConnected

  /**
    * In directed graph it's a path
    */
  override lazy val admitEulerianChainOrPath: Boolean =
    isConnected && getNodes.map(e => (getSuccessorsIds(e), getPredecessorsIds(e))).filterNot(e => e._1.size == e._2.size).size <= 2

  /**
    * Is eulerian means that it contains an eulerian circuit
    */
  override lazy val isEulerian: Boolean =
    isConnected && getNodes.map(e => (getSuccessorsIds(e), getPredecessorsIds(e))).forall(e => e._1.size == e._2.size)

  /**
    * Check the strong connectivity of the graph
    */
  lazy val isStrongConnected: Boolean =
    if (nodesIds.isEmpty) false
    else depthFirstSearch(nodesIds.head).size == nbNodes && inverse.depthFirstSearch(nodesIds.head).size == nbNodes

  override def getLinks: List[Arc[T]] = arcs

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
}