package fr.clementgarbay.graph
package adjacency_list

/**
  * @author Clément Garbay
  * @author Anaël Chardan
  * @tparam T The type of the node id
  */
case class AdjacencyListDirectedGraph[T](nodes: List[NodeDirected[T]]) extends IDirectedGraph[T] {

  override lazy val nbNodes: Int = nodes.size
  override lazy val arcs: List[Arc[T]] =
    nodes.flatMap(node => node.successors.map(successor => Arc(node.id, successor.to, successor.distance)))
  override lazy val nbArcs: Int = nodes.map(_.successors.size).sum // arcs.size
  override lazy val nodesIds: List[T] = nodes.map(_.id)

  override lazy val toAdjacencyMatrix: Map[T, Map[T, Int]] =
    nodes.map(from =>
      from.id -> nodes.map(to => to.id -> isArc(from.id, to.id).toInt).toMap
    ).toMap

  override lazy val inverse: IDirectedGraph[T] =
    AdjacencyListDirectedGraph(
      nodes.map(node => NodeDirected(node.id, getPredecessors(node.id)))
    )

  override lazy val toUndirectedGraph: AdjacencyListUndirectedGraph[T] =
    AdjacencyListUndirectedGraph(nodes.map(node => {
      NodeUndirected(node.id, node.successors.map(n => SemiEdge(n.to, n.distance)) ++ nodes
        .filter(_.successorsIds contains node.id)
        .map(nodeDirected => SemiEdge(nodeDirected.id, nodeDirected.distanceWith(node.id).get)))
    }))

  override def getNodes: List[T] = nodes.map(_.id)

  override def isArc(from: T, to: T): Boolean = nodes.exists(node => node.id == from && node.successorsIds.contains(to))

  override def addArc(from: T, to: T, distance: Double): IDirectedGraph[T] =
    if (from == to) this
    else AdjacencyListDirectedGraph(nodes.map {
      case node if node.id == from =>
        node.copy(successors = node.successors + SemiArc(to, distance))
      case node => node
    })

  override def addArc(from: T, to: T): IDirectedGraph[T] =
    addArc(from, to, 1.0)

  override def removeArc(from: T, to: T): IDirectedGraph[T] =
    AdjacencyListDirectedGraph(nodes.map {
      case node if node.id == from =>
        node.copy(successors = node.successors.filterNot(_.to == to))
      case node => node
    })

  override def getDistance(from: T, to: T): Double =
    nodes.find(_.id == from).flatMap(node => node.successors.find(_.to == to).map(_.distance)).getOrElse(0)

  override def getSuccessors(nodeId: T): Set[SemiArc[T]] = nodes.find(_.id == nodeId).map(_.successors).getOrElse(Set.empty)

  override def getSuccessorsIds(nodeId: T): Set[T] = getSuccessors(nodeId).map(_.to)

  override def getPredecessors(nodeId: T): Set[SemiArc[T]] =
    nodes.filter(_.successorsIds contains nodeId).map(node => SemiArc(node.id, node.successors.find(_.to == nodeId).get.distance)).toSet

  override def getPredecessorsIds(nodeId: T): Set[T] = nodes.filter(_.successorsIds contains nodeId).map(_.id).toSet

}

object AdjacencyListDirectedGraph {

  implicit def apply(matrix: => List[List[Int]]): AdjacencyListDirectedGraph[Int] =
    AdjacencyListDirectedGraph(matrix.zipWithIndex.collect {
      case (successors, j) => NodeDirected(j, successors.zipWithIndex.collect {
        case (value, i) if value == 1 => SemiArc(i)
      }.toSet)
    })

  def fromMatrix[T](matrix: => Map[T, Map[T, Int]]): AdjacencyListDirectedGraph[T] = {
    AdjacencyListDirectedGraph(matrix.map {
      case (j, successors) => NodeDirected(j, successors.collect {
        case (i, value) if value == 1 => SemiArc(i)
      }.toSet)
    }.toList)
  }

  implicit def apply[T](adjacencyList: Map[T, Set[T]]): AdjacencyListDirectedGraph[T] =
    AdjacencyListDirectedGraph(adjacencyList.map {
      case (id, successors) => NodeDirected(id, successors)
    }.toList)


  def fromMapWithDistances[T](adjacencyList: => Map[T, Set[(T, Double)]]): AdjacencyListDirectedGraph[T] =
    AdjacencyListDirectedGraph(adjacencyList.map {
      case (id, successors) => NodeDirected(id, successors.map {
        case (successor, distance) => SemiArc(successor, distance)
      })
    }.toList)

}
