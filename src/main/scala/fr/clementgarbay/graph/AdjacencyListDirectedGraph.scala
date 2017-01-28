package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  *
  * @tparam T The type of the node id
  */
case class AdjacencyListDirectedGraph[T](nodes: Set[NodeDirected[T]]) extends IDirectedGraph[T] {

  override lazy val nbNodes: Int = nodes.size
  override lazy val arcs: Set[(T, T, Double)] =
    nodes.flatMap(node => node.successors.map(successor => (node.id, successor._1, successor._2)))
  override lazy val nbArcs: Int = nodes.toList.map(_.successors.size).sum // arcs.size
  override lazy val nodesIds: Set[T] = nodes.map(_.id)

  override lazy val toAdjacencyMatrix: List[List[Int]] = ???
//    (0 until nodes.size).map { i =>
//      (0 until nodes.size).map { j =>
//        isArc(i,j).toInt
//      }.toList
//    }.toList

  override lazy val inverse: IDirectedGraph[T] =
    AdjacencyListDirectedGraph(
      nodes.map(node => NodeDirected(node.id, getPredecessors(node.id)))
    )

  override lazy val toUndirectedGraph: AdjacencyListUndirectedGraph[T] =
    AdjacencyListUndirectedGraph(nodes.map(node => {
      NodeUndirected(node.id, node.successors ++ nodes
        .filter(_.successorsIds contains node.id)
        .map(nodeDirected => (nodeDirected.id, nodeDirected.distanceWith(node.id).get)))
    }))

  override def isArc(from: T, to: T): Boolean = nodes.exists(node => node.id == from && node.successorsIds.contains(to))

  override def addArc(from: T, to: T, distance: Double): IDirectedGraph[T] =
    if (from == to) this
    else AdjacencyListDirectedGraph(nodes.map {
      case node if node.id == from =>
        node.copy(successors = node.successors + ((to, distance)))
      case node => node
    })

  override def addArc(from: T, to: T): IDirectedGraph[T] =
    addArc(from, to, 1.0)

  override def removeArc(from: T, to: T): IDirectedGraph[T] =
    AdjacencyListDirectedGraph(nodes.map {
      case node if node.id == from =>
        node.copy(successors = node.successors.filterNot(_._1 == to))
      case node => node
    })

  override def getSuccessors(nodeId: T): Set[(T, Double)] = nodes.find(_.id == nodeId).map(_.successors).getOrElse(Set.empty)

  override def getSuccessorsIds(nodeId: T): Set[T] = getSuccessors(nodeId).map(_._1)

  override def getPredecessors(nodeId: T): Set[(T, Double)] =
    nodes.filter(_.successorsIds contains nodeId).map(node => (node.id, node.successors.find(_._1 == nodeId).get._2))

  override def getPredecessorsIds(nodeId: T): Set[T] = nodes.filter(_.successorsIds contains nodeId).map(_.id)

}

object AdjacencyListDirectedGraph {

  implicit def apply(matrix: List[List[Int]]): AdjacencyListDirectedGraph[Int] =
    AdjacencyListDirectedGraph(matrix.zipWithIndex.collect({
      case (successors, j) => NodeDirected(j, successors.zipWithIndex.collect({
        case (value, i) if value == 1 => (i, 1.0)
      }).toSet)
    }).toSet)

  implicit def apply[T](adjacencyList: Map[T, Set[T]]): AdjacencyListDirectedGraph[T] =
    AdjacencyListDirectedGraph(adjacencyList.map({
      case (id, successors) => NodeDirected(id, successors)
    }).toSet)

}
