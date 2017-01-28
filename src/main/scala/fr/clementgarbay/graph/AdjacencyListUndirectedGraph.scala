package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
case class AdjacencyListUndirectedGraph[T](nodes: Set[NodeUndirected[T]]) extends IUndirectedGraph[T] {

  override lazy val nbNodes: Int = nodes.size
  override lazy val nbEdges: Int = nodes.toList.map(_.neighbors.size).sum / 2
  override lazy val nodesIds: Set[T] = nodes.map(_.id)

  override lazy val toAdjacencyMatrix: List[List[Int]] = ???
//    (0 until nodes.size).map { i =>
//      (0 until nodes.size).map { j =>
//        (isEdge(i, j) || isEdge(j, i)).toInt
//      }.toList
//    }.toList

  override def isEdge(from: T, to: T): Boolean = nodes.exists(node => node.id == from && node.neighborsIds.contains(to))

  override def addEdge(from: T, to: T, distance: Double): IUndirectedGraph[T] =
    if (from == to) this
    else AdjacencyListUndirectedGraph(nodes.map {
      case node if node.id == from =>
        node.copy(neighbors = node.neighbors + ((to, distance)))
      case node if node.id == to =>
        node.copy(neighbors = node.neighbors + ((from, distance)))
      case node => node
    })

  override def addEdge(from: T, to: T): IUndirectedGraph[T] =
    addEdge(from, to, 1.0)

  override def removeEdge(from: T, to: T): IUndirectedGraph[T] =
    AdjacencyListUndirectedGraph(nodes.map {
      case node if node.id == from =>
        node.copy(neighbors = node.neighbors.filterNot(_ == to))
      case node if node.id == to =>
        node.copy(neighbors = node.neighbors.filterNot(_ == from))
      case node => node
    })

  override def getNeighbors(nodeId: T): Set[(T, Double)] = nodes.find(_.id == nodeId).map(_.neighbors).getOrElse(Set.empty)

  override def getNeighborsIds(nodeId: T): Set[T] = getNeighbors(nodeId).map(_._1)

}

object AdjacencyListUndirectedGraph {

  implicit def apply(matrix: List[List[Int]]): AdjacencyListUndirectedGraph[Int] = {
    AdjacencyListUndirectedGraph(matrix.zipWithIndex.map({
      case (neighbors, j) => NodeUndirected(j, neighbors.zipWithIndex.collect({
        case (value, i) if value == 1 => (i, 1.0)
      }).toSet)
    }).toSet)
  }

  implicit def apply[T](adjacencyList: Map[T, Set[T]]): AdjacencyListUndirectedGraph[T] = {
    AdjacencyListUndirectedGraph(adjacencyList.map({
      case (id, neighbors) => NodeUndirected(id, neighbors)
    }).toSet)
  }

  implicit def apply[T](adjacencyListDirected: AdjacencyListDirectedGraph[T]): AdjacencyListUndirectedGraph[T] = {
    adjacencyListDirected.toUndirectedGraph
  }

}