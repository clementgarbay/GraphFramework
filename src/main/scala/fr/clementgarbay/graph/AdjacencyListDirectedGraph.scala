package fr.clementgarbay.graph

/**
  * @author Clément Garbay
  */
case class AdjacencyListDirectedGraph(nodes: Set[NodeDirected]) extends IDirectedGraph {

  override lazy val nbNodes: Int = nodes.size
  override lazy val nbArcs: Int = nodes.toList.map(_.successors.size).sum

  override def isArc(from: Int, to: Int): Boolean = nodes.exists(node => node.id == from && node.successors.contains(to))

  override def addArc(from: Int, to: Int): IDirectedGraph =
    AdjacencyListDirectedGraph(nodes.map {
      case node if node.id == from =>
        node.copy(successors = node.successors + to)
      case node => node
    })

  override def removeArc(from: Int, to: Int): IDirectedGraph =
    AdjacencyListDirectedGraph(nodes.map {
      case node if node.id == from =>
        node.copy(successors = node.successors.filterNot(_ == to))
      case node => node
    })

  override def getSuccessors(node: Int): Set[Int] = nodes.find(_.id == node).map(_.successors).getOrElse(Set.empty)

  override def getPredecessors(node: Int): Set[Int] = nodes.filter(_.successors contains node).map(_.id)

  override def computeInverse: IDirectedGraph =
    AdjacencyListDirectedGraph(
      nodes.map(node => NodeDirected(node.id, getPredecessors(node.id)))
    )

  override def toAdjacencyMatrix: List[List[Int]] =
    (0 until nodes.size).map { i =>
      (0 until nodes.size).map { j =>
        isArc(i,j).toInt
      }.toList
    }.toList

  /**
    * @return An undirected graph from this directed graph
    */
  def toUndirectedGraph: AdjacencyListUndirectedGraph =
    AdjacencyListUndirectedGraph(nodes.map(node => {
      val neighbors: Set[Int] = node.successors ++ nodes.filter(_.successors contains node.id).map(_.id)
      NodeUndirected(node.id, neighbors)
    }))

}

object AdjacencyListDirectedGraph {

  def apply(matrix: List[List[Int]]): AdjacencyListDirectedGraph = {
    new AdjacencyListDirectedGraph(matrix.zipWithIndex.map({
      case (successors, j) => NodeDirected(j, successors.zipWithIndex.collect({
        case (value, i) if value == 1 => i
      }).toSet)
    }).toSet)
  }

}
