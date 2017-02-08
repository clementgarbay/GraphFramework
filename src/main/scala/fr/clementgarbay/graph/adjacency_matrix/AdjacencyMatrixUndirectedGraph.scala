package fr.clementgarbay.graph
package adjacency_matrix

/**
  * @author Clément Garbay
  * @author Anaël Chardan
  *
  * TODO: with distances
  */
case class AdjacencyMatrixUndirectedGraph[T](matrix: Map[T, Map[T, Int]]) extends IUndirectedGraph[T] {

  override val nbNodes: Int = matrix.size
  override lazy val edges: List[Edge[T]] =
    matrix.collect {
      case (from, neighbors) => neighbors.collect {
        case (to, value) if value == 1 => List(Edge(from, to), Edge(to, from))
      }.toSet.flatten
    }.flatten.toList

  override lazy val nbEdges: Int = matrix.map(_._2.count(_._2 == 1)).sum / 2 // edges.size
  override lazy val nodesIds: List[T] = matrix.keys.toList

  override val toAdjacencyMatrix: Map[T, Map[T, Int]] = matrix

  override def getNodes: List[T] = matrix.keys.toList

  override def isEdge(from: T, to: T): Boolean =
    matrix.lift(from).flatMap(_.lift(to)).getOrElse(0) == 1 ||
    matrix.lift(to).flatMap(_.lift(from)).getOrElse(0) == 1

  override def addEdge(from: T, to: T): IUndirectedGraph[T] = update(from, to, 1)

  override def addEdge(from: T, to: T, distance: Double): IUndirectedGraph[T] = addEdge(from, to)

  override def removeEdge(from: T, to: T): IUndirectedGraph[T] = update(from, to, 0)

  override def getNeighbors(nodeId: T): Set[SemiEdge[T]] = getNeighborsIds(nodeId).map(node => SemiEdge(node))

  override def getNeighborsIds(nodeId: T): Set[T] =
    matrix.lift(nodeId).getOrElse(Map.empty).collect {
      case (node, isArc) if isArc == 1 => node
    }.toSet

  private def update(from: T, to: T, value: Int): IUndirectedGraph[T] =
    AdjacencyMatrixUndirectedGraph(matrix.map {
      case (index, neighbors) if index == from && neighbors.contains(to) => (index, neighbors.updated(to, value))
      case (index, neighbors) if index == to && neighbors.contains(from) => (index, neighbors.updated(from, value))
      case (index, neighbors) => (index, neighbors)
    })
}

object AdjacencyMatrixUndirectedGraph {

  implicit def apply[T](graph: IUndirectedGraph[T]): AdjacencyMatrixUndirectedGraph[T] =
    AdjacencyMatrixUndirectedGraph(
      graph.getNodes.map(from => from -> graph.getNodes.map(to => (to, graph.isEdge(from, to).toInt)).toMap).toMap
    )

  implicit def apply(matrix: List[List[Int]]): AdjacencyMatrixUndirectedGraph[Int] = {
    AdjacencyMatrixUndirectedGraph(matrix.zipWithIndex.map(_.swap).toMap.mapValues(_.zipWithIndex.map(_.swap).toMap))
  }
}