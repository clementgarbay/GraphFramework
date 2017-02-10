package fr.clementgarbay.graph

import scala.collection.mutable

/**
  * @author Clément Garbay
  * @author Anaël Chardan
  */
trait IUndirectedGraph[T] extends IGraph[T, SemiEdge[T]] {

  /**
    * A list of edges with starting node, ending node and the distance between both
    */
  val edges: List[Edge[T]]

  /**
    * The number of edges in the graph
    */
  val nbEdges: Int

  override lazy val inverse: IGraph[T, SemiEdge[T]] = this

  override lazy val isConnected: Boolean =
    if (nodesIds.isEmpty) false
    else depthFirstSearch(nodesIds.head).size == nbNodes

  /**
    * On undirected graph it's a chain
    */
  override lazy val admitEulerianChainOrPath: Boolean = {
    lazy val oddDegresNodes = getNodes.map(getNeighborsIds).count(_.size % 2 == 0)
    isConnected && (oddDegresNodes == 0 || oddDegresNodes == 2)
  }

  /**
    * Is eulerian means that it contains an eulerian cycle
    */
  override val isEulerian: Boolean = {
    lazy val oddDegresNodes = getNodes.map(getNeighborsIds).count(_.size % 2 == 0)
    isConnected && (oddDegresNodes == 0)
  }

  override def getLinks: List[Edge[T]] = edges

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
    * @inheritdoc
    */
  override def getNextNodes(nodeId: T): Set[SemiEdge[T]] = getNeighbors(nodeId)

  /**
    * @inheritdoc
    */
  override def getNextNodesId(nodeId: T): Set[T] = getNeighborsIds(nodeId)




  def getColororifiedNode: Map[Int, List[T]] = {
    var availableColors = (0 to nbNodes).toList
    var coloredNodes: mutable.Map[T, Option[Int]] = mutable.Map.empty[T, Option[Int]]
    nodesIds.foreach(e => coloredNodes.update(e, None))


    val s = new mutable.Stack[T]()
    var node = nodesIds.head
    var result: List[T] = List.empty

    s.push(node)
    while (s.nonEmpty) {
      node = s.pop()
      result = result :+ node
      var takenColors: List[Int] = getNeighbors(node).map(e => coloredNodes(e.to)).collect {
        case Some(x) => x
      }.toList
      var firstAvailableColor = availableColors.filter(e => !takenColors.contains(e)).head

      coloredNodes.update(node, Some(firstAvailableColor))

      getNextNodes(node).foreach(e => if (!result.contains(e.to) && !s.contains(e.to)) s.push(e.to))
    }

    coloredNodes.groupBy(_._2).map(e => e._1.get -> e._2.keys.toList)
  }
}
