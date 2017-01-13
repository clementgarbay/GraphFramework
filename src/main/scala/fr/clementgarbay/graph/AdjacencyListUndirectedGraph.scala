package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
case class AdjacencyListUndirectedGraph(nodes: Set[NodeUndirected]) extends IUndirectedGraph {

  override lazy val nbNodes: Int = nodes.size
  override lazy val nbEdges: Int = nodes.toList.map(_.neighbors.size).sum / 2

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

  override def toAdjacencyMatrix: List[List[Int]] =
    (0 until nodes.size).map { i =>
      (0 until nodes.size).map { j =>
        (isEdge(i, j) || isEdge(j, i)).toInt
      }.toList
    }.toList

}

object AdjacencyListUndirectedGraph {

  def apply(matrix: List[List[Int]]): AdjacencyListUndirectedGraph = {
    new AdjacencyListUndirectedGraph(matrix.zipWithIndex.map({
      case (neighbors, i) => NodeUndirected(i, neighbors.filter(_ == 1).toSet)
    }).toSet)
  }

}