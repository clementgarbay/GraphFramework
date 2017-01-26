package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
case class AdjacencyListUndirectedGraph(nodes: Set[NodeUndirected]) extends IUndirectedGraph {

  override lazy val nbNodes: Int = nodes.size
  override lazy val nbEdges: Int = nodes.toList.map(_.neighbors.size).sum / 2
  override lazy val nodesList: Set[Int] = nodes.map(_.id)

  override lazy val toAdjacencyMatrix: List[List[Int]] =
    (0 until nodes.size).map { i =>
      (0 until nodes.size).map { j =>
        (isEdge(i, j) || isEdge(j, i)).toInt
      }.toList
    }.toList

  override def isEdge(from: Int, to: Int): Boolean = nodes.exists(node => node.id == from && node.neighbors.contains(to))

  override def addEdge(from: Int, to: Int): IUndirectedGraph =
    AdjacencyListUndirectedGraph(nodes.map {
      case node if node.id == from =>
        node.copy(neighbors = node.neighbors + to)
      case node if node.id == to =>
        node.copy(neighbors = node.neighbors + from)
      case node => node
    })

  override def removeEdge(from: Int, to: Int): IUndirectedGraph =
    AdjacencyListUndirectedGraph(nodes.map {
      case node if node.id == from =>
        node.copy(neighbors = node.neighbors.filterNot(_ == to))
      case node if node.id == to =>
        node.copy(neighbors = node.neighbors.filterNot(_ == from))
      case node => node
    })

  override def getNeighbors(node: Int): Set[Int] = nodes.find(_.id == node).map(_.neighbors).getOrElse(Set.empty)

}

object AdjacencyListUndirectedGraph {

  implicit def apply(matrix: List[List[Int]]): AdjacencyListUndirectedGraph = {
    AdjacencyListUndirectedGraph(matrix.zipWithIndex.map({
      case (neighbors, j) => NodeUndirected(j, neighbors.zipWithIndex.collect({
        case (value, i) if value == 1 => i
      }).toSet)
    }).toSet)
  }

  implicit def apply(adjacencyList: Map[Int, Set[Int]]): AdjacencyListUndirectedGraph = {
    AdjacencyListUndirectedGraph(adjacencyList.map({
      case (id, neighbors) => NodeUndirected(id, neighbors)
    }).toSet)
  }

  implicit def apply(adjacencyListDirected: AdjacencyListDirectedGraph): AdjacencyListUndirectedGraph = {
    AdjacencyListUndirectedGraph(adjacencyListDirected.nodes.map(node => {
      NodeUndirected(node.id, node.successors ++ adjacencyListDirected.nodes.filter(_.successors contains node.id).map(_.id))
    }))
  }

}