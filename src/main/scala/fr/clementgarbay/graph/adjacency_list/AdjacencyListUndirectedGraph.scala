package fr.clementgarbay.graph
package adjacency_list

/**
  * @author Clément Garbay
  * @author Anaël Chardan
  */
case class AdjacencyListUndirectedGraph[T](nodes: List[NodeUndirected[T]]) extends IUndirectedGraph[T] {

  override lazy val nbNodes: Int = nodes.size
  override lazy val edges: List[Edge[T]] =
    nodes.flatMap(node => node.neighbors.map(neighbor => {
      List(Edge(node.id, neighbor.to, neighbor.distance), Edge(neighbor.to, node.id, neighbor.distance))
    })).flatten
  override lazy val nbEdges: Int = nodes.map(_.neighbors.size).sum / 2
  override lazy val nodesIds: List[T] = nodes.map(_.id)

  override lazy val toAdjacencyMatrix: Map[T, Map[T, Int]] =
    nodes.map(from =>
      from.id -> nodes.map(to => to.id -> (isEdge(from.id, to.id) || isEdge(to.id, from.id)).toInt).toMap
    ).toMap

  override def getNodes: List[T] = nodes.map(_.id)

  override def isEdge(from: T, to: T): Boolean = nodes.exists(node => node.id == from && node.neighborsIds.contains(to))

  override def addEdge(from: T, to: T, distance: Double): IUndirectedGraph[T] =
    if (from == to) this
    else AdjacencyListUndirectedGraph(nodes.map {
      case node if node.id == from =>
        node.copy(neighbors = node.neighbors + SemiEdge(to, distance))
      case node if node.id == to =>
        node.copy(neighbors = node.neighbors + SemiEdge(from, distance))
      case node => node
    })

  override def addEdge(from: T, to: T): IUndirectedGraph[T] =
    addEdge(from, to, 1.0)

  override def removeEdge(from: T, to: T): IUndirectedGraph[T] =
    AdjacencyListUndirectedGraph(nodes.map {
      case node if node.id == from =>
        node.copy(neighbors = node.neighbors.filterNot(_.to == to))
      case node if node.id == to =>
        node.copy(neighbors = node.neighbors.filterNot(_.to == from))
      case node => node
    })

  override def getNeighbors(nodeId: T): Set[SemiEdge[T]] = nodes.find(_.id == nodeId).map(_.neighbors).getOrElse(Set.empty)

  override def getNeighborsIds(nodeId: T): Set[T] = getNeighbors(nodeId).map(_.to)

}

object AdjacencyListUndirectedGraph {

  implicit def apply(matrix: => List[List[Int]]): AdjacencyListUndirectedGraph[Int] = {
    AdjacencyListUndirectedGraph(matrix.zipWithIndex.map({
      case (neighbors, j) => NodeUndirected(j, neighbors.zipWithIndex.collect({
        case (value, i) if value == 1 => SemiEdge(i)
      }).toSet)
    }))
  }

  def fromMatrix[T](matrix: => Map[T, Map[T, Int]]): AdjacencyListUndirectedGraph[T] = {
    AdjacencyListUndirectedGraph(matrix.map {
      case (j, neighbors) => NodeUndirected(j, neighbors.collect {
        case (i, value) if value == 1 => SemiEdge(i)
      }.toSet)
    }.toList)
  }

  implicit def apply[T](adjacencyList: Map[T, Set[T]]): AdjacencyListUndirectedGraph[T] = {
    AdjacencyListUndirectedGraph(adjacencyList.map({
      case (id, neighbors) => NodeUndirected(id, neighbors)
    }).toList)
  }

  implicit def apply[T](adjacencyListDirected: AdjacencyListDirectedGraph[T]): AdjacencyListUndirectedGraph[T] = {
    adjacencyListDirected.toUndirectedGraph
  }

  def fromMapWithDistances[T](adjacencyList: => Map[T, Set[(T, Double)]]): AdjacencyListUndirectedGraph[T] =
    AdjacencyListUndirectedGraph(adjacencyList.map {
      case (id, successors) => NodeUndirected(id, successors.map {
        case (successor, distance) => SemiEdge(successor, distance)
      })
    }.toList)
}